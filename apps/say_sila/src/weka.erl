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
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(weka).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([report_to_arff/2,
         report_to_arff/3,
         tweets_to_arff/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("raven.hrl").
-include("twitter.hrl").

-define(put_attr(FOut, Attrib, Type),   io:format(FOut, "@ATTRIBUTE ~s ~s\n",    [Attrib, Type])).
-define(put_attr(FOut, A0, A1, Type),   io:format(FOut, "@ATTRIBUTE ~s_~s ~s\n", [A0, A1, Type])).
-define(put_nl(FOut),                   io:put_chars(FOut, "\n")).

% TODO: Move to llog
-define(fmt(Fmt, Args), io_lib:format(Fmt, Args)).


%%====================================================================
%% API
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
    lists:map(fun(Type) -> report_to_arff(?fmt("~s.~s", [Name, Type]), Type, RptMap) end,
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
    io:put_chars(FOut, "\n@DATA\n"),
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
                ?put_nl(FOut),
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
    io:put_chars(FOut, "\n@DATA\n"),
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
                 FOut   :: term()) -> ok
                                  | {error, term()}.
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

