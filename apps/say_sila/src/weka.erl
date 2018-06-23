%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Sila Weka functionality
%%
%%      Weka runs on the JVM and Sila uses a Clojure/OTP node to
%%      communicate with it.  This module contains the utilities to
%%      prepare for handoffs to Weka.
%%
%%      ARFF formatting corresponds to the requirements of the package
%%      AffectiveTweets, a plugin for Weka:
%%
%%      REF: https://github.com/felipebravom/AffectiveTweets
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(weka).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([biggies_to_arff/3,
         report_to_arff/2,
         report_to_arff/3,
         tweets_to_arff/2]).

-include("sila.hrl").
-include("ioo.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include("player.hrl").
-include_lib("llog/include/llog.hrl").

-define(put_data(FOut),                 io:put_chars(FOut, "\n@DATA\n")).
-define(put_attr(FOut, Attrib, Type),   io:format(FOut, "@ATTRIBUTE ~s ~s\n",    [Attrib, Type])).
-define(put_attr(FOut, A0, A1, Type),   io:format(FOut, "@ATTRIBUTE ~s_~s ~s\n", [A0, A1, Type])).



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec biggies_to_arff(Name    :: string(),
                      Biggies :: proplist(),
                      Players :: any()) ->  {ok, string()}
                                         |  {{error, term()}, string()}.
%%
% @doc  Generates a set of ARFF (Attribute-Relation File Format) relations
%       for the output of `players:get_biggies'.
%
%       NOTE: This function is addressing the question of influence in
%             Twitter communities, and is currently somewhat in flux.
% @end  --
biggies_to_arff(Name, Biggies, Players) ->

    % Use the "all-tweets" category as a time-slice reference,
    % but not for the ARFF output. (It is just tweets+retweets.)
    BigCommCodes = proplists:delete(tter, ?COMM_CODES),
    RegCommCodes = [oter],
    InitComm  = ?NEW_COMM,

    % Remove ALL categories of big players to get our regular players
    BigAccter = fun(Code) ->
                    {_, _, Accts} = proplists:get_value(Code, Biggies),
                    Accts end,

    AllBigAccts = lists:flatten([BigAccter(Code) || Code <- BigCommCodes]),
    RegPlayers  = maps:without(AllBigAccts, Players),

    % Create a new proplist with {code, BP-lots, RP-lots}
    %
    % NOTE: `lots' looks like map(K=day, V=map(K=code, V=comm))
    Updater  = fun(DTS, AcctComms, {Code, LotsAcc}) ->
                   %
                   % Make sure the user has this kind of communication data
                   NewLotsAcc = case maps:get(Code, AcctComms, undefined) of
                       undefined ->
                           %?warning("~s<~B>: NO-COMMS", [Code, DTS]),
                           LotsAcc;
                       AcctComm  ->
                           % We have user comm data for this code,
                           % pull the matching comm record from the accumulator
                           LotsComms    = maps:get(DTS,  LotsAcc, #{}),
                           LotsComm     = maps:get(Code, LotsComms, InitComm),
                           NewLotsComm  = player:update_comm(LotsComm, AcctComm),
                           NewLotsComms = maps:put(Code, NewLotsComm, LotsComms),
                           maps:put(DTS, NewLotsComms, LotsAcc)
                   end,
                   {Code, NewLotsAcc}
                   end,

    % Function to run through all the DTS lots for a user, adding each to a lot accumulator
    Allotter = fun(_User, #profile{lots = UserLots}, {Code, LotsAcc}) ->
                   %?notice("Player lots: acct[~s] code[~s] lots[~B]", [User, Code, maps:size(UserLots)]),
                   {_,
                    NewLotsAcc} = maps:fold(Updater, {Code, LotsAcc}, UserLots),
                   {Code, NewLotsAcc}
                   end,

    % Function to split the accounts into big|regular players for each comm-code
    DeCommer = fun(Code, Grp) ->
                   GrpPlayers = case Grp of
                       reg -> RegPlayers;
                       big -> maps:with(BigAccter(Code), Players)
                   end,
                   ?info("Compiling ~s_~s communications", [Grp, Code]),
                   {_,
                    Lots} = maps:fold(Allotter, {Code, #{}}, GrpPlayers),
                   {Code, Lots}
                   end,

    BigLots = [DeCommer(Code, big) || Code <- BigCommCodes],
    RegLots = [DeCommer(Code, reg) || Code <- RegCommCodes],

    % Sanity check: for run periods of significant size, all lots should be the same size.
    Checker = fun(Code, {Grp, GrpLots}) ->
                      Lots = proplists:get_value(Code, GrpLots),
                      ?debug("LOTS: comm[~s] grp[~s] size[~B]", [Code, Grp, maps:size(Lots)]),
                      {Grp, GrpLots}
                      end,

    lists:foldl(Checker, {big, BigLots}, BigCommCodes),
    lists:foldl(Checker, {reg, RegLots}, RegCommCodes),

    % Now we have our data organized the way we need it for the ARFF.  Let's go!
    {FPath, FOut} = open_arff(Name),

    Attribber = fun({Grp, Code, Emo}) ->
                    Attr = ?str_fmt("~s_~s_~s", [Grp, Code, Emo]),
                    ?put_attr(FOut, Attr, numeric)
                    end,
    lists:foreach(Attribber, [{big, Code, Emo} || Code <- BigCommCodes, Emo <- ?EMOTIONS]),
    lists:foreach(Attribber, [{reg, Code, Emo} || Code <- RegCommCodes, Emo <- ?EMOTIONS]),

    % Function to write one emotion for one comm on one line of the ARFF
    Emoter = fun(Emo, Levels) ->
                 Val = maps:get(Emo, Levels),
                 ?io_fmt(FOut, ",~f", [Val]),
                 Levels end,

    % Function to write one comm on one line of the ARFF
    Commer = fun(Code, {DTS, GrpLots}) ->
                 Lots = proplists:get_value(Code, GrpLots),
                 %
                 % TODO: This line may fail for very small periods.
                 %        Decide how we want to handle missing bits.
                 Comms = maps:get(DTS, Lots),
                 #comm{emos = Emos} = maps:get(Code, Comms),
                 %
                 % We're not really reducing, the "accumulator" just holds the emo-map
                 lists:foldl(Emoter, Emos#emotions.levels, ?EMOTIONS),
                 {DTS, GrpLots} end,

    % Function to write one line of the ARFF
    Liner  = fun(DTS) ->
                 % The DTS is a millisecond timestamp for the current lot in the period
                 ?io_fmt(FOut, "~B", [DTS]),
                 lists:foldl(Commer, {DTS, BigLots}, BigCommCodes),
                 lists:foldl(Commer, {DTS, RegLots}, RegCommCodes),
                 ?io_nl(FOut)
                 end,

    % We use original tweets to get our DTS keys, but all the other categories must match
    ?put_data(FOut),
    Template = proplists:get_value(oter, BigLots),
    lists:foreach(Liner, maps:keys(Template)),

    close_arff(FPath, FOut).



%%--------------------------------------------------------------------
-spec report_to_arff(Name   :: string(),
                     RptMap :: map()) -> [{ok, string()}]
                                       | [{{error, term()}, string()}].
%%
% @doc  Generates a set of ARFF (Attribute-Relation File Format) relations
%       for a Big/Regular player `raven' report.
% @end  --
report_to_arff(Name, RptMap = #{big := BigRptPack}) ->
    %
    % Run each of the big report types (the reg reports should match)
    lists:map(fun(Type) -> report_to_arff(?str_fmt("~s.~s", [Name, Type]), Type, RptMap) end,
              proplists:get_keys(BigRptPack)).



%%--------------------------------------------------------------------
-spec report_to_arff(Name   :: string(),
                     Type   :: atom(),
                     RptMap :: map()) -> {ok, string()}
                                       | {{error, term()}, string()}.
%%
% @doc  Generate an ARFF (Attribute-Relation File Format) relation for
%       a Big/Regular player `raven' report.
% @end  --
report_to_arff(Name, Type, #{big := BigRptPack,
                             reg := RegRptPack}) ->
    %
    % TODO: We'll want an ARFF per emotion
    BigRpt = proplists:get_value(Type, BigRptPack),
    RegRpt = proplists:get_value(Type, RegRptPack),

    ?info("Converting ~s report: rel[~s] cat[~s:~s] acct[~B:~B] tt[~B:~B]",
          [Type,
           Name,
           BigRpt#report.category,      RegRpt#report.category,
           BigRpt#report.num_players,   RegRpt#report.num_players,
           BigRpt#report.num_tweets,    RegRpt#report.num_tweets]),

    {FPath, FOut} = open_arff(Name),

    ?put_attr(FOut, dts, string),
    lists:foreach(fun(Var) ->
                      ?put_attr(FOut, big, Var, numeric),
                      ?put_attr(FOut, reg, Var, numeric)
                      end,
                  [cnt | ?EMOTIONS]),

    % Create one data instance per day
    ?put_data(FOut),
    BigEmos = BigRpt#report.emotions,
    RegEmos = RegRpt#report.emotions,
    MakeInst = fun(Key, BigLot, {LotCnt, BigCnt, RegCnt}) ->
        DTS = dts:str(Key),
        case maps:get(Key, RegEmos, nak) of
            nak ->
                ?warning("No regular tweets on ~s", [DTS]),
                {LotCnt + 1, BigCnt, RegCnt};
            RegLot ->
                BigLotCnt = BigLot#emotions.count,
                RegLotCnt = RegLot#emotions.count,
                io:format(FOut, "~s, ~B, ~B", [DTS, BigLotCnt, RegLotCnt]),
                %?debug("BIG: ~p", [BigLot]),
                %?debug("REG: ~p", [RegLot]),
                lists:foreach(fun(Emo) ->
                                  io:format(FOut, ", ~f, ~f",
                                            [maps:get(Emo, BigLot#emotions.levels),
                                             maps:get(Emo, RegLot#emotions.levels)])
                                  end,
                              ?EMOTIONS),
                ?io_nl(FOut),
                {LotCnt + 1,
                 BigCnt + BigLotCnt,
                 RegCnt + RegLotCnt}
            end
        end,
    {LotCount,
     BigTotal,
     RegTotal} = maps:fold(MakeInst, {0, 0, 0}, BigEmos),

    ?info("Processed ~B lots: big[~B] reg[~B]", [LotCount, BigTotal, RegTotal]),
    close_arff(FPath, FOut).



%%--------------------------------------------------------------------
-spec tweets_to_arff(Name   :: string(),
                     Tweets :: [tweet()]) -> {ok, string()}
                                           | {{error, term()}, string()}.
%%
% @doc  Convert a list of tweets to an ARFF (Attribute-Relation File Format)
%       file for Weka.
%
%       Sample Weka target from https://github.com/felipebravom/EmoInt :
% ```
%       java -Xmx4G -cp $HOME/weka-3-8-1/weka.jar weka.Run weka.classifiers.meta.FilteredClassifier
%           -t data/anger-ratings-0to1.train.arff
%           -T data/anger-ratings-0to1.test.target.arff
%           -classifications "weka.classifiers.evaluation.output.prediction.CSV
%               -use-tab
%               -p first-last
%               -file data/anger-pred.csv"
%           -F "weka.filters.MultiFilter
%               -F \"weka.filters.unsupervised.attribute.TweetToEmbeddingsFeatureVector
%                   -I 2
%                   -B $HOME/wekafiles/packages/AffectiveTweets/resources/w2v.twitter.edinburgh.100d.csv.gz
%                   -S 0 -K 15 -L -O\"
%               -F \"weka.filters.unsupervised.attribute.TweetToLexiconFeatureVector
%                   -I 2 -A -D -F -H -J -L -N -P -Q -R -T -U -O\"
%               -F \"weka.filters.unsupervised.attribute.TweetToSentiStrengthFeatureVector
%                   -I 2 -U -O\"
%               -F \"weka.filters.unsupervised.attribute.Reorder
%                   -R 5-last,4\""
%           -W weka.classifiers.functions.LibLINEAR -- -S 12 -C 1.0 -E 0.001 -B 1.0 -L 0.1 -I 1000
% '''
% @end  --
tweets_to_arff(Name, Tweets) ->
    %
    {FPath, FOut} = open_arff(Name),

    ?put_attr(FOut, id,          string),
    ?put_attr(FOut, screen_name, string),
    ?put_attr(FOut, text,        string),

    % Fill in the tweet text as attribute data
    ?put_data(FOut),
    write_tweets(FOut, Tweets),

    % All ready for some learnin'
    close_arff(FPath, FOut).




%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec open_arff(Name :: string()) -> {string(), term()}.
%%
% @doc  Opens an ARFF file for writing the specified relation and
%       returns the full pathname to the file and its file descriptor.
% @end  --
open_arff(Name) ->
    %
    FPath = make_fpath(Name),
    {ok, FOut} = file:open(FPath, [write]),

    io:format(FOut, "@RELATION  ~s~n~n", [Name]),
    {FPath, FOut}.



%%--------------------------------------------------------------------
-spec close_arff(FPath  :: string(),
                 FOut   :: pid()) -> {ok, string()}
                                   | {{error, atom()}, string()}.
%%
% @doc  Closes an ARFF file, previously opened with `open_arff'.
% @end  --
close_arff(FPath, FOut) ->
    FStatus = file:close(FOut),
    ?info("ARFF<create>: path[~s] stat[~p]", [FPath, FStatus]),
    {FStatus, FPath}.



-spec make_fpath(Name :: string()) -> string().
%%
% @doc  Returns the full filepath for an ARFF file with the specified
%       name.  Note that the path must be valid for this function to
%       succeed.
% @end  --
make_fpath(Name) ->
    FPath = lists:flatten(io_lib:format("~s/weka/~s.arff", [?WORK_DIR, Name])),
    ok = filelib:ensure_dir(FPath),
    FPath.



%%--------------------------------------------------------------------
-spec write_tweets(Out    :: pid(),
                   Tweets :: [[tweet()]]) -> ok.
%%
% @doc  Write out a list of tweets to an ARFF file.
% @end  --
write_tweets(_, []) ->
    ok;

write_tweets(Out, [Tweet = #tweet{text = Text0} | Rest]) ->
    %
    % So Weka doesn't freak:
    Text1 = re:replace(Text0, "(\\\\')|(')", [$\\, $\\, $'], [global]),     % Correct/escape single quotes
    Text  = re:replace(Text1, "[\r\n]", " ", [global, {return, binary}]),   % Linefeeds/newlines to spaces
    io:format(Out, "'~s','~s','~s'~n", [Tweet#tweet.id,
                                        Tweet#tweet.screen_name,
                                        Text]),
    write_tweets(Out, Rest).

