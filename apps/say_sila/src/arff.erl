%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Sila ARFF utilities for Weka models
%%
%%      Note that Weka runs on the JVM and Sila uses a Clojure/OTP node
%%      to communicate with it.  This module contains the utilities to
%%      prepare datasets for handoff to Weka.
%%
%%      Tweet-related ARFF formatting corresponds to the requirements of the package
%%      AffectiveTweets, a plugin for Weka:
%%
%%      REF: https://github.com/felipebravom/AffectiveTweets
%%
%% @copyright 2017-2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(arff).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([from_biggies/4,
         from_biggies/6,
         from_report/2,
         from_report/3,
         from_tweets/2,         from_tweets/3,
         get_big_comm_codes/0,
         make_attribute/3]).

-include("sila.hrl").
-include("ioo.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include("player.hrl").
-include_lib("llog/include/llog.hrl").

-define(WEKA_DIR, ?WORK_DIR "/weka").
-define(put_data(FOut),                 io:put_chars(FOut, "\n@DATA\n")).
-define(put_attr(FOut, Attrib, Type),   io:format(FOut, "@ATTRIBUTE ~s ~s\n",    [Attrib, Type])).
-define(put_attr(FOut, A0, A1, Type),   io:format(FOut, "@ATTRIBUTE ~s_~s ~s\n", [A0, A1, Type])).

-type arff_return() :: {ok, string()}
                     | {cont, file:io_device()}
                     | {{error, term()}, string()}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec from_biggies(Name    :: stringy(),
                   RegCode :: comm_code(),
                   Biggies :: proplist(),
                   Players :: any()) ->  {ok, string()}
                                      |  {{error, term()}, string()}.
%%
% @doc  Generates a set of ARFF (Attribute-Relation File Format) relations
%       for the output of `players:get_biggies'.  Each output instance
%       represents one day during a tracking run, and the ARFF includes
%       (target) attributes for all defined emotions.
%
% @end  --
from_biggies(Name, RegCode, Biggies, Players) ->
    from_biggies(Name, RegCode, ?EMOTIONS, Biggies, Players, 1).



%%--------------------------------------------------------------------
-spec from_biggies(Name    :: stringy(),
                   RegCode :: comm_code(),
                   RegEmos :: emotion()
                            | emotions(),
                   Biggies :: proplist(),
                   Players :: any(),
                   Period  :: integer()) ->  {ok, string()}
                                          |  {{error, term()}, string()}.
%%
% @doc  Generates a set of ARFF (Attribute-Relation File Format) relations
%       for the output of `players:get_biggies'.  Each output instance
%       corresponds to the number of days in `Period' over the course of
%       a tracking run.
%
%       NOTE: This function is addressing the question of influence in
%             Twitter communities, and is currently somewhat in flux.
% @end  --
from_biggies(Name, RegCode, RegEmo, Biggies, Players, Period) when is_atom(RegEmo) ->
    from_biggies(Name, RegCode, [RegEmo], Biggies, Players, Period);


from_biggies(Name, RegCode, RegEmos, Biggies, Players, Period) ->

    % Use the "all-tweets" category as a time-slice reference,
    % but not for the ARFF output. (It is just tweets+retweets.)
    BigCodes = get_big_comm_codes(),
    RegCodes = [RegCode],
    InitComm = ?NEW_COMM,

    % Separate out comm-code lots for big & regular players, regrouping lots if necessary
    {BigLots,
     RegLots} = case {Period,
                      make_biggie_lots(BigCodes, RegCodes, Biggies, Players, InitComm)} of

        % Default is one instance per day
        {1, {BigDayLots, RegDayLots}} -> {BigDayLots, RegDayLots};

        % Otherwise, group by days (usually 7 for a week)
        {N, {BigDayLots,
             RegDayLots}} when N > 1 ->
            %
            % Use the regular community to find ALL the days we're considering.
            % Depending on biggie requirements, some commumications (rter) may be missing days!
            Days = lists:sort(maps:keys(proplists:get_value(RegCode, RegDayLots))),

            {[{Code, periodize_lots(Code, DayLots, N, Days, InitComm)} || {Code, DayLots} <- BigDayLots],
             [{Code, periodize_lots(Code, DayLots, N, Days, InitComm)} || {Code, DayLots} <- RegDayLots]}
    end,

    % Now we have our data organized the way we need it for the ARFF.  Let's go!
    {FPath, FOut} = init_biggie_arff(Name, BigCodes, RegCodes, ?EMOTIONS, RegEmos),

    write_biggie_arff(FOut, BigCodes, RegCodes, BigLots, RegLots, ?EMOTIONS, RegEmos, InitComm),
    close_arff(FPath, FOut).



%%--------------------------------------------------------------------
-spec from_report(Name   :: string(),
                     RptMap :: map()) -> [{ok, string()}]
                                       | [{{error, term()}, string()}].
%%
% @doc  Generates a set of ARFF (Attribute-Relation File Format) relations
%       for a Big/Regular player `raven' report.
% @end  --
from_report(Name, RptMap = #{big := BigRptPack}) ->
    %
    % Run each of the big report types (the reg reports should match)
    lists:map(fun(Type) -> from_report(?str_fmt("~s.~s", [Name, Type]), Type, RptMap) end,
              proplists:get_keys(BigRptPack)).



%%--------------------------------------------------------------------
-spec from_report(Name   :: string(),
                     Type   :: atom(),
                     RptMap :: map()) -> {ok, string()}
                                       | {{error, term()}, string()}.
%%
% @doc  Generate an ARFF (Attribute-Relation File Format) relation for
%       a Big/Regular player `raven' report.
% @end  --
from_report(Name, Type, #{big := BigRptPack,
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
                BigLotCnt = BigLot#emos.count,
                RegLotCnt = RegLot#emos.count,
                io:format(FOut, "~s, ~B, ~B", [DTS, BigLotCnt, RegLotCnt]),
                %?debug("BIG: ~p", [BigLot]),
                %?debug("REG: ~p", [RegLot]),
                lists:foreach(fun(Emo) ->
                                  io:format(FOut, ", ~f, ~f",
                                            [maps:get(Emo, BigLot#emos.levels),
                                             maps:get(Emo, RegLot#emos.levels)])
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
-spec from_tweets(Name   :: stringable(),
                  Tweets :: [tweet()]) -> arff_return().

-spec from_tweets(Name   :: stringable(),
                  Tweets :: [tweet()]
                          | [map()],
                  Opts   :: options()) -> arff_return().
%%
% @doc  Convert a list of tweets to an ARFF (Attribute-Relation File Format)
%       file for Weka.
%
%       Available options are:
%       - {mode, single}                : The whole ARFF will be this one list of tweets (default)
%       - {mode, start}                 : Begin continuous ARFF creation
%       - {mode, {cont, FOut, FName}}   : Continue ongoing ARFF creation
%       - {mode, {stop, FOut, FName}    : End and finalize ARFF creation
%       - {target, attr}                : Append the specified attribute (Tweets must be in map format)
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
from_tweets(Name, Tweets) ->
    from_tweets(Name, Tweets, []).


from_tweets(Name, Tweets, Opts) ->

    % Are they asking for a specialized target value?
    Target = pprops:get_value(target, Opts, none),

    % Function to begin the ARFF file
    OpenARFF = fun() ->
        {FP, Out} = open_arff(tweets, Name),

        ?info("Creating ARFF: ~s", [FP]),
        ?put_attr(Out, id,          string),
        ?put_attr(Out, lang,        string),
        ?put_attr(Out, screen_name, string),
        ?put_attr(Out, name,        string),
        ?put_attr(Out, description, string),
        ?put_attr(Out, text,        string),

        % TODO: Add an option setting for non-string target attributes
        case Target of
            none -> ok;
            _    -> ?put_attr(Out, Target, string)
        end,

        ?put_data(Out),
        {FP, Out}
    end,

    % Did they send us a file device, or are we starting from scratch?
    {{FPath, FOut} = FInfo,
     CloseNow} = case pprops:get_value(mode, Opts, single) of
        single            -> {OpenARFF(), true};
        start             -> {OpenARFF(), false};
        {cont, {FP, Out}} -> {{FP, Out},  false};
        {stop, {FP, Out}} -> {{FP, Out},  true}
    end,

    % Fill in the tweet text as attribute data
    write_tweets(FOut, Tweets, Target),

    % Are they going to be sending more, or are we all ready?
    case CloseNow of
        true  -> close_arff(FPath, FOut);
        false -> {cont, FInfo}
    end.



%%--------------------------------------------------------------------
-spec get_big_comm_codes() -> comm_codes().
%%
% @doc  Returns the list of communication codes we consider for big
%       player activity.
% @end  --
get_big_comm_codes() ->
    proplists:delete(tter, ?COMM_CODES).



%%--------------------------------------------------------------------
-spec make_attribute(Group :: big|reg,
                     Code  :: comm_code(),
                     Emo   :: emotion()) -> atom().
%%
% @doc  Creates a Weka attribute name from the specified player group,
%       communication code and emotion.
% @end  --
make_attribute(Group, Code, Emo) ->
    list_to_atom(?str_FMT("~s_~s_~s", [Group, Code, Emo])).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec open_arff(Name :: stringy()) -> {string(), file:io_device()}.
%%
% @doc  Opens an ARFF file for writing the specified relation and
%       returns the full pathname to the file and its file descriptor.
% @end  --
open_arff(Name) ->
    %
    FPath = ioo:make_fpath(?WEKA_DIR, Name, <<"arff">>),
    {ok, FOut} = file:open(FPath, [write]),

    io:format(FOut, "@RELATION  ~s~n~n", [Name]),
    {FPath, FOut}.



%%--------------------------------------------------------------------
-spec open_arff(SubDir :: stringy(),
                Name   :: stringy()) -> {string(), file:io_device()}.
%%
% @doc  Opens an ARFF file in the specified local subdirectory for
%       the named relation.  Returns the full filepath and the file
%       descriptor.
% @end  --
open_arff(SubDir, Name) ->
    LocalFPath = ?str_fmt("~s/~s", [SubDir, Name]),
    open_arff(LocalFPath).



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



%%--------------------------------------------------------------------
-spec write_tweets(Out    :: pid(),
                   Tweets :: [tweet()]
                           | [map()],
                   Target :: stringable()) -> ok.
%%
% @doc  Write out a list of tweets to an ARFF file.
% @end  --
write_tweets(_, [], _) ->
    ok;

write_tweets(Out, [Tweet|Rest], Target) ->

    % TODO: I am considering deprecating the record form because I keep adding to it.
    %       Therefore, pending a decision, a Target attribute here requires the map form.
    %
    % The tweet may be in record or raw map form.
    TwRec = case Tweet of
        R = #tweet{}     -> R;
        M when is_map(M) -> tweet:from_map(M)
    end,

    % Function to clean text so that Weka doesn't freak:
    Clean = fun(T0) ->
        case T0 of
            undefined -> <<>>;
            _ ->
                T1 = re:replace(T0, "(\\\\')|(['])", [$\\, $\\, $'], [global]),         % Fix/escape single quotes
                     re:replace(T1, "[\r\n]|(\\\\$)", " ", [global, {return, binary}])  % NLs/LFs/trailing '\'s
        end
    end,

    ?io_fmt(Out, "'~s','~s','~s','~s','~s','~s'", [TwRec#tweet.id,
                                                   TwRec#tweet.lang,
                                                   TwRec#tweet.screen_name,
                                                   Clean(TwRec#tweet.name),
                                                   Clean(TwRec#tweet.description),
                                                   Clean(TwRec#tweet.text)]),

    % Currently, a Target attribute requires the map form
    case Target of
        none -> ?io_put(Out, "\n");
        _    -> ?io_fmt(Out, ",'~s'~n", [maps:get(Target, Tweet)])      % TODO: Support non-string targets
    end,
    write_tweets(Out, Rest, Target).



%%--------------------------------------------------------------------
-spec make_biggie_lots(BigCommCodes  :: [atom()],
                       RegCommmCodes :: [atom()],
                       Biggies       :: proplist(),
                       Players       :: any(),
                       InitComm      :: comm()) -> {proplist(),
                                                    proplist()}.
%%
% @doc  Separates the comm-code lots for the big and regular players
% @end  --
make_biggie_lots(BigCommCodes, RegCommCodes, Biggies, Players, InitComm) ->

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

    % Prepare big/reg lots by comm-code, one per day
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
    {BigLots, RegLots}.


%%--------------------------------------------------------------------
-spec periodize_lots(CommCode :: comm_code(),
                     DayLots  :: map(),
                     Period   :: non_neg_integer(),
                     Days     :: [dts_1970()],
                     InitComm :: comm()) -> map().
%%
% @doc  Regroup day-mapped lots to period-mapped lots (key = first day).
%       The `Period' is a span of N days (usually 7 for a week).
%
%       NOTE: Lots for extra days that don't make up a full period are ignored.
%
% @end  --
periodize_lots(CommCode, DayLots, Period, Days, InitComm) ->

    % NOTE: Erlang/OTP 21 has map iterators, which may provide a more elegant
    %       solution, provided that the keys are processed in order.
    GroupDays = fun(Day, {N, CurrGroup, AllGroups}) ->
                    %
                    case N >= Period of
                        % Finalize this group, and go to the next
                        true ->
                            {1, [], [[Day|CurrGroup] | AllGroups]};

                        % Add this day to the current group and keep going
                        false  ->
                            {N + 1, [Day | CurrGroup], AllGroups}
                    end end,

    % Function to create a period->lot mapping from a day->lot map
    RemapDays = fun(PeriodDays, PeriodLotAcc) ->
                    %
                    % Function to merge a day lot into an accumulating period lot
                    Reemoter = fun(Code, DayComm, LotAcc) ->
                                   %?debug("Re-emoting: code[~s] comm~p", [Code, DayComm]),
                                   AccComm = maps:get(Code, LotAcc, InitComm),
                                   NewEmos = emo:average(AccComm#comm.emos,
                                                         DayComm#comm.emos),
                                   % FIXME: Don't duplicate counts in #comm and #emos!
                                   %        For now, this is a good place for a SANITY check.
                                   NewComm = case {NewEmos#emos.count,
                                                   AccComm#comm.cnt + DayComm#comm.cnt} of

                                       {Cnt, Cnt} ->
                                           % FIXME: Here we where we see the double-count nonsense
                                           AccComm#comm{cnt  = NewEmos#emos.count,
                                                        emos = NewEmos};

                                       {EmoCnt, CommCnt} ->
                                           ?error("Emotions/comms count mismatch: code[~s] emo[~B] cnt[~B]",
                                                  [CommCode, EmoCnt, CommCnt]),
                                           throw(bad_count)
                                   end,
                                   maps:put(Code, NewComm, LotAcc)
                                   end,

                    % Function to warn about empty lots
                    Check00s = fun(Code, Comm, Day) ->
                                   case Comm#comm.cnt > 0 of
                                       true  -> ok;
                                       false ->
                                           ?warning("Empty ~s comm on ~s <~B>: ~p",
                                                    [Code, dts:date_str(Day, millisecond), Day]),
                                           ?debug("~p", [Comm])
                                   end end,

                    % Function to map a list of day lots into one period lot
                    Remapper = fun(Day, {_, LotAcc}) ->
                                   % Day-order is reversed, so the last day becomes the DTS key
                                   Remap = maps:fold(Reemoter, LotAcc, maps:get(Day, DayLots, #{})),
                                   case maps:size(Remap) of
                                       0 ->
                                           ?warning("Empty ~s day @ ~s <~B>",
                                                    [CommCode, dts:date_str(Day, millisecond), Day]);
                                       1 ->
                                           %?debug("Remapped ~s @ ~s <~B>",
                                           %         [CommCode, dts:date_str(Day, millisecond), Day]),
                                           % Sanity check against comms w/ no tweets!
                                           maps:fold(Check00s, Day, Remap),
                                           ok
                                   end,
                                   {Day, Remap} end,

                    % Remap the day lots by day groupings
                    %?info("Remapping ~s period: ~p",
                    %      [CommCode, [dts:date_STR(Day, millisecond) || Day <- PeriodDays]]),
                    {KeyDay,
                     PeriodLots} = lists:foldl(Remapper, {start, #{}}, PeriodDays),
                    maps:put(KeyDay, PeriodLots, PeriodLotAcc)
                    end,

    % Regroup day-mapped lots to period-mapped lots (key = first day)
    {_,
     ExtraDays,
     Periods} = lists:foldl(GroupDays, {1, [], []}, Days),
%%   Periods} = lists:foldl(GroupDays, {1, [], []}, lists:sublist(Days, 50, 7)),    % DEBUG: subset
    case ExtraDays of
        [] -> ok;
        _  -> ?warning("Days cut from ~s period groupings: ~p",
                       [CommCode, [dts:date_STR(Day, millisecond) || Day <- ExtraDays]])
    end,
    lists:foldr(RemapDays, #{}, Periods).



%%--------------------------------------------------------------------
-spec init_biggie_arff(Name     :: stringy(),
                       BigCodes :: comm_codes(),
                       RegCodes :: comm_codes(),
                       BigEmos  :: emotions(),
                       RegEmos  :: emotions()) -> {string(), file:io_device()}.
%%
% @doc  Opens an ARFF for biggie influence analysis and writes out the
%       attribute header.
% @end  --
init_biggie_arff(Name, BigCodes, RegCodes, BigEmos, RegEmos) ->

    % Now we have our data organized the way we need it for the ARFF.  Let's go!
    Return = {_, FOut} = open_arff(Name),

    ?put_attr(FOut, minute, date),
    Attribber = fun({Grp, Code, Emo}) ->
                    Attr = make_attribute(Grp, Code, Emo),
                    ?put_attr(FOut, Attr, numeric)
                    end,
    lists:foreach(Attribber, [{big, Code, Emo} || Code <- BigCodes, Emo <- BigEmos]),
    lists:foreach(Attribber, [{reg, Code, Emo} || Code <- RegCodes, Emo <- RegEmos]),
    Return.



%%--------------------------------------------------------------------
-spec write_biggie_arff(FOut     :: file:io_device(),
                        BigCodes :: comm_codes(),
                        RegCodes :: comm_codes(),
                        BigLots  :: proplist(),
                        RegLots  :: proplist(),
                        BigEmos  :: emotions(),
                        RegEmos  :: emotions(),
                        InitComm :: comm()) -> ok.
%%
% @doc  Opens an ARFF for biggie influence analysis and writes out the
%       attribute header.
% @end  --
write_biggie_arff(FOut, BigCodes, RegCodes, BigLots, RegLots, BigEmos, RegEmos, InitComm) ->

    % Function to write one emotion for one comm on one line of the ARFF
    Emoter = fun(Emo, Levels) ->
                 Val = maps:get(Emo, Levels),
                 ?io_fmt(FOut, ",~f", [Val]),
                 Levels end,

    % Function to write one comm on one line of the ARFF
    Commer = fun(Code, {DTS, Grp, GrpLots, Emotions}) ->
                 Lots = proplists:get_value(Code, GrpLots),
                 %
                 % TODO: This line may fail for very small periods.
                 %        Decide how we want to handle missing bits.
                 Comms = case maps:get(DTS, Lots, none) of
                    none ->
                        ?warning("Missing lot: grp[~s] comm[~s] ms[~B] dts[~s]",
                                 [Grp, Code, DTS, dts:str(DTS, millisecond)]),
                    #{Code => InitComm};
                    Lot -> Lot
                 end,
                 #comm{emos = Emos} = maps:get(Code, Comms),
                 %
                 % We're not really reducing, the "accumulator" just holds the emo-map
                 lists:foldl(Emoter, Emos#emos.levels, Emotions),
                 {DTS, Grp, GrpLots, Emotions} end,

    % Function to write one line of the ARFF
    Liner  = fun(DTS) ->
                 % The DTS is a millisecond timestamp for the current lot in the period
                 ?io_put(FOut, dts:str(DTS, millisecond)),
                 lists:foldl(Commer, {DTS, big, BigLots, BigEmos}, BigCodes),
                 lists:foldl(Commer, {DTS, reg, RegLots, RegEmos}, RegCodes),
                 ?io_nl(FOut)
                 end,

    % We use original tweets to get our DTS keys, but all the other categories must match
    ?put_data(FOut),
    Template = proplists:get_value(oter, BigLots),
    lists:foreach(Liner, maps:keys(Template)).

