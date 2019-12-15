%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila code relating to research on the "Big Players"
%%
%% @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(biggies).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([period/1]).
-export([make_h0/2,
         run_top_n/2,  run_top_n/3,
         run_top_nn/2, run_top_nn/3]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(MIN_H0_TWEETS,  40).                % Low values make for empty rter|tmed days

-type dataset()       :: parms | train | test.
-type verification()  :: ack | nak | undefined.
-type verifications() :: [verification()].

-type run_code() :: n | nn.
%type run_fun()  :: fun((tracker(), stringy(), comm_code(), emotion(), proplist()) -> proplist()).
-type method()   :: {run_code(), {atom, pos_integer(), pos_integer}}.


%%--------------------------------------------------------------------
-spec period(DataTag :: dataset()) -> proplist().
%%
%% Shortcut to get tracking run period.
%%
%% RUN-2:   May 2018 -- Aug 2018                        (parms)
%%          Sep 2018 -- Aug 2019                        (train)
%%          Sep 2019 -- Dec 2019                        (test)
%%--------------------------------------------------------------------
-define(RUN, 2).
%% biggies:run_top_n(gw, run2L, [{data_mode, level}]).      biggies:run_top_nn(gw, run2LL, [{data_mode, level}]).
%% biggies:run_top_n(gw, run2V, [{data_mode, variation}]).  biggies:run_top_nn(gw, run2VV, [{data_mode, variation}]).
%%--------------------------------------------------------------------
-if(?RUN =:= 2).
%period(parms) -> [{start, {2018, 05, 01}}, {stop, {2018, 09, 01}}];
%period(train) -> [{start, {2018, 09, 01}}, {stop, {2019, 09, 01}}];
%period(test)  -> [{start, {2019, 09, 01}}, {stop, {2019, 12,  8}}]. % TODO: goto 2020/1/1

%period(parms) -> [{start, {2018, 09, 01}}, {stop, {2018, 12, 01}}];
%period(train) -> [{start, {2018, 12, 01}}, {stop, {2019, 09, 01}}];
%period(test)  -> [{start, {2019, 09, 01}}, {stop, {2019, 12,  1}}]. % TODO: goto 2020/1/1

%period(parms) -> [{start, {2017, 09, 01}}, {stop, {2017, 12, 01}}];
%period(train) -> [{start, {2017, 12, 01}}, {stop, {2018, 09, 01}}];
%period(test)  -> [{start, {2018, 09, 01}}, {stop, {2019, 01, 01}}].

%%--------------------------------------------------------------------
%% Best RUN-2 so far
%%--------------------------------------------------------------------
period(parms) -> [{start, {2017, 10, 01}}, {stop, {2018, 01, 01}}];
period(train) -> [{start, {2018, 01, 01}}, {stop, {2018, 10, 01}}];
period(test)  -> [{start, {2018, 10, 01}}, {stop, {2019, 01, 01}}].
%%--------------------------------------------------------------------

% variation:
% A @
% F @
% S @
% J @
%period(parms) -> [{start, {2018, 06, 01}}, {stop, {2018, 09, 01}}];
%period(train) -> [{start, {2018, 09, 01}}, {stop, {2019, 06, 01}}];
%period(test)  -> [{start, {2019, 06, 01}}, {stop, {2019, 09, 01}}].
-else.
%%--------------------------------------------------------------------
%% RUN-1: October 1, 2017 -- June 30, 2018              (10-fold CV)
%%--------------------------------------------------------------------
%eriod(parms) -> [{start, {2019, 01, 01}}, {stop, {2019, 04, 01}}];
period(parms) -> [{start, {2017, 09, 01}}, {stop, {2017, 12, 01}}]; % FIXME: overlap!
period(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 07, 01}}]; % FIXME!
%eriod(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 04, 01}}]; % FIXME! shortie
period(test)  -> [{start, {2018, 04, 01}}, {stop, {2018, 07, 01}}].
%eriod(test)  -> [{start, {2019, 10, 01}}, {stop, {2019, 12, 31}}].
-endif.


%%--------------------------------------------------------------------
-spec make_h0(Biggies :: proplist(),
              Players :: map()) -> proplist().
%%
% @doc  Selects a set of null hypothesis (H0) "medium" players for
%       comparison runs.
% @end  --
make_h0(Biggies, Players) ->
    % Ref: http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf
    %rand:seed_s(exsss),

    % Combine the big player accounts from all categories except `tter'
    AllBigs = lists:foldl(fun ({_, {_,_,Bigs}}, Acc) -> Bigs ++ Acc end,
                          [],
                          Biggies),

    % Function to turn a big-comm entry into a medium-comm entry
    FindMedComm = fun ({Comm, {_,_,Bigs}}) ->
        % We want to choose from users with some level of activity that are not big players
        % Fold the usernames into a map indexed by non-negative integers for random retrieval
        FindMediums = fun(Usr, Info, Acc = {Cnt, Meds}) ->
            IsMedium = case maps:get(Comm, Info#profile.comms, none) of
                none -> false;
                CCnt -> (CCnt#comm.cnt >= ?MIN_H0_TWEETS) andalso (not lists:member(Usr, AllBigs))
            end,

            % Toss the biggies and the extremely inactive; otherwise, include this guy.
            case IsMedium of
                false -> Acc;
                true  -> {Cnt+1, maps:put(Cnt, Usr, Meds)}
            end
        end,
        {MediumCnt,
         MedPlayers} = maps:fold(FindMediums, {0, #{}}, Players),
        MaxMedIndex  = MediumCnt - 1,

        ?info("Choosing H0 from ~B ~s players with at least ~B tweet(s)",
              [MediumCnt, Comm, ?MIN_H0_TWEETS]),

        % Function to get a random medium player
        ChooseMediums = fun
            Recur([], Acc) ->
                Acc;
            Recur(Redo = [_|Rest], Acc) ->
                Ndx = round(MaxMedIndex * rand:uniform()),
                Usr = maps:get(Ndx, MedPlayers),
                % FIXME: We'll recur forever if there aren't enough mediums
                case lists:member(Usr, Acc) of
                    false -> Recur(Rest, [Usr|Acc]);        % good: first-time player
                    true  -> Recur(Redo, Acc)               % oops, already picked this one
                end
        end,

        Meds = ChooseMediums(Bigs, []),
        ?info("Medium ~s: ~B players", [Comm, length(Meds)]),
        {Comm, {0.0,0,Meds}}
    end,
    [FindMedComm(B) || B <- Biggies].



%%--------------------------------------------------------------------
-spec run_top_n(Tracker :: tracker(),
                RunTag  :: stringy()) -> verifications().

-spec run_top_n(Tracker :: tracker(),
                RunTag  :: stringy(),
                Options :: proplist()) -> verifications().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       Supported options are:
%       - `period'      : Number of days per data instance (default: 7)
%       - `data_mode'   : The `level'(default) mode uses the raw emotion
%                         values to create models, while `variation' use
%                         the difference from one time step to the next.
%       - `learner'     : Learning algorithm.  Choose:
%                           `lreg' (default) for LinearRegression, or
%                           `gproc' for GaussianProcesses.
% @end  --
run_top_n(Tracker, RunTag) ->
    run_top_n(Tracker, RunTag, []).


run_top_n(Tracker, RunTag, Options) ->
    run_run(n, Tracker, RunTag, Options).



%%--------------------------------------------------------------------
-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy()) -> verifications().

-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy(),
                 Options :: proplist()) -> verifications().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This `nn' function goes through the Top-N twice, using only
%       the emo/comm attributes from the higher ranking models.
%
%       Supported options are:
%       - `period'      : Number of days per data instance (default: 7)
%       - `data_mode'   : The `level'(default) mode uses the raw emotion
%                         values to create models, while `variation' use
%                         the difference from one time step to the next.
% @end  --
run_top_nn(Tracker, RunTag) ->
    run_top_nn(Tracker, RunTag, []).


run_top_nn(Tracker, RunTag, Options) ->
    run_run(nn, Tracker, RunTag, Options).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec prep_data(RunCode :: run_code(),
                Tracker :: tracker(),
                Options :: proplist()) -> {method(), map(), map(), map()}.
%%
% @doc  Prepares the `train', `parms' and  `test' datasets as well as
%       the Big and Medium (null hypothesis) player communities.
% @end  --
prep_data(RunCode, Tracker, Options) ->

    Method = {RunCode, {top_n, Min, Max} = influence:init_range(top_n)},

    % Function to pull and organize players and their tweets for a given dataset
    GetPlayers = fun(D) ->
        sila:reset(),
        ok = raven:emote(Tracker, period(D)),
        wait_on_players(Tracker),
        player:get_players(Tracker)
    end,
    Players = GetPlayers(train),

    % The training dataset gives the player community that deines the Top-N groups
    TopBiggies = maps:from_list([{N, player:get_top_n(Tracker, N)} || N <- lists:seq(Min, Max)]),
    TopMediums = maps:map(fun(_, Bigs) -> make_h0(Bigs, Players) end,
                          TopBiggies),

    % The `parms_pct' option specifies the percentage of training data to reserve
    % for parameter optimization.  0% means we should use an independent dataset.
    ParmsData = case proplists:get_value(parms_data, Options, 0) of
        0    -> GetPlayers(parms);
        P100 -> P100
    end,

    % The `test' dataset player community is always independent
    DataSets = maps:from_list([{train, Players},
                               {parms, ParmsData},
                               {test,  GetPlayers(test)}]),
    {Method, DataSets, TopBiggies, TopMediums}.



%%--------------------------------------------------------------------
-spec run_run(RunCode :: run_code(),
              Tracker :: tracker(),
              RunTag  :: stringy(),
              Options :: proplist()) -> verifications().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.  But first
%       warn the user that this will reset Say-Sila.
% @end  --
run_run(RunCode, Tracker, RunTag, Options) ->
    io:format("This will destroy the current raven and player states.~n"),
    case ioo:read_down("Are you sure? ") of
        "yes" ->
            case weka:ping(raven:get_jvm_node(Tracker)) of
                timeout -> [];
                _       -> do_run_run(RunCode, Tracker, RunTag, Options)
            end;
        _ -> []
    end.



%%--------------------------------------------------------------------
-spec do_run_run(RunCode :: run_code(),
                 Tracker :: tracker(),
                 RunTag  :: stringy(),
                 Options :: proplist()) -> verifications().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
% @end  --
do_run_run(RunCode, Tracker, RunTag, Options) ->

    % We'll be calling into one of the `influence' run functions
    RunFun = case RunCode of
        n  -> fun influence:run_top_n/5;
        nn -> fun influence:run_top_nn/5
    end,

    % We need Big and Medium (H0) player communities.  "Ref" here means "null hypothesis".
    {Method,
     DataSets,
     TopBiggies,
     TopMediums} = prep_data(RunCode, Tracker, Options),
    DataMode = proplists:get_value(data_mode, Options, level),
    RunOpts  = [{datasets, DataSets}, {toppers,  TopBiggies} | Options],
    RefOpts  = [{datasets, DataSets}, {toppers,  TopMediums} | Options],
    RefTag   = ?str_fmt("~s_H0", [RunTag]),

    Totals = #{tter := Count} = wait_on_players(Tracker),
    ?notice("Completed processing ~p tweets", [Count]),
    maps:map(fun(Comm, Cnt) ->
                 ?info("* ~s: ~B", [Comm, Cnt]) end,
             Totals),

    % Function to create and run models for all emotions
    Process = fun(Tag, Opts) ->
        [{Emo, RunFun(Tracker, Tag, oter, Emo, Opts)} || Emo <- ?EMOTIONS]
    end,

    % Process sets of run and reference models
    RunResults = Process(RunTag, RunOpts),
    RefResults = Process(RefTag, RefOpts),

    %
    Report = fun
        Recur([], [], Verifications) ->
            Verifications;
        Recur([{Emo, Run} | RestRuns],
              [{Emo, Ref} | RestRefs], Verifications) ->
            V = report_run(Tracker, Method, DataMode, Emo, Options, Run, Ref),
            Recur(RestRuns, RestRefs, [V|Verifications])
    end,
    Report(RunResults, RefResults, []).



%%--------------------------------------------------------------------
-spec report_run(Tracker    :: tracker(),
                 Method     :: tuple(),
                 DataMode   :: data_mode(),
                 Emotion    :: emotion(),
                 Options    :: proplist(),
                 RunResults :: proplist(),
                 RefResults :: proplist()) -> verification().
%%
% @doc  Hold processing until Weka has returned all tweet batches and
% @end  --
report_run(Tracker, Method, DataMode, Emotion, Options, RunResults, RefResults) ->

    % Announce the report and log the time periods
    ?notice("Reporting '~s' run for ~s", [DataMode, Emotion]),
    LogStamper = fun(Step) ->
        Period = period(Step),
        [?info("Period ~-5s: ~s__~s", [Step | [dts:str(proplists:get_value(T, Period)) || T <- [start,stop]]])]
    end,
    [LogStamper(Step) || Step <- [parms, train, test]],

    % Function to check a value and add a warning to a collection if it is not correct
    Checker = fun(Step, N, Score, Data, Warnings) ->
         case Score of
            S when is_float(S)  -> {?str_fmt("~7.4f", [S]), Warnings};
            need_data           -> {"*******",              [{Step, N, Data} | Warnings]}
        end
    end,

    % Report the results, collecting the warnings so we can log them all after the report
    Report = fun
        Recur([], [], Warnings) ->
            Warnings;
        Recur([{N, {RunVal, RunData}} | RunRest],
              [{N, {RefVal, RefData}} | RefRest], Warnings) ->
            % We may not have had enough data for a reference score
            {RunPCC, MidWarnings} = Checker(run, N, RunVal, RunData, Warnings),
            {RefPCC, NewWarnings} = Checker(ref, N, RefVal, RefData, MidWarnings),
            ?info("N @ ~2B: pcc[~s] ref[~s] model~p", [N, RunPCC, RefPCC, RunData]),
            Recur(RunRest, RefRest, NewWarnings)
    end,
    Warnings = Report(RunResults, RefResults, []),

    % And report the warnings
    Warner = fun({Step, N, CntMap}) ->
        CntTxt = [?str_fmt(" ~s[~6.4f]", [Comm, Pct]) || {Comm, Pct} <- maps:to_list(CntMap)],
        ?warning("No reference ~s model #~2B:~s", [Step, N, CntTxt])
    end,
    [Warner(W) || W <- lists:reverse(Warnings)],

    verify_run(Tracker, Method, DataMode,  oter, Emotion, Options, RunResults).



%%--------------------------------------------------------------------
-spec wait_on_players(Tracker :: tracker()) -> map().
%%
% @doc  Hold processing until Weka has returned all tweet batches and
%       the player server for the specified `Tracker' has finished its
%       processing.  The function returns the totals reported by the
%       player server.
% @end  --
wait_on_players(Tracker) ->

    % We may still be waiting on messages from Weka
    Waiter = fun Recur() ->
        case raven:count_tweet_todo(Tracker) of
            0 -> ok;
            N ->
                ?info("Waiting on tweets: days[~p]", [N]),
                timer:sleep(1000),
                Recur()
        end
    end,
    Waiter(),

    % Be patient.  The `player' module may have to churn a while
    ?info("Waiting for player processing to complete..."),
    player:get_totals(Tracker, infinity).



%%--------------------------------------------------------------------
-spec verify_run(Tracker  :: tracker(),
                 Method   :: tuple(),
                 DataMode :: data_mode(),
                 Comm     :: comm_code(),
                 Emo      :: emotion(),
                 Options  :: proplist(),
                 Results  :: proplist()) -> verification().
%%
% @doc  Generates a hash representing the results of a given run.
%       If we know what the results should be (we have a hash from
%       a previous run), then we compare the current run's results
%       and warn the user they don't match up.
% @end  --
verify_run(Tracker, Method, DataMode, Comm, Emo, Options, Results) ->

    % Function to create a hexstring SHA-256 fingerprint
    Hash = fun(Elm) ->
        lists:flatten([io_lib:format("~.16B", [X]) || <<X>> <= crypto:hash(sha256, term_to_binary(Elm))])
    end,

    Cfg = [{run,?RUN}, Tracker, Method, DataMode, Comm, Emo | lists:sort(Options)],
    Key = Hash(Cfg),
    Val = Hash(Results),

    ?info("RUN % ~p: ~p => ~p,", [Cfg, Key, Val]),
    case get_run_hash(Key) of
        none -> ?warning("No previous results to compare"),                         undefined;
        Val  -> ?notice("Results are consistent!"),                                 ack;
        Prev -> ?error("Results do not match previous runs: prev[~p]", [Prev]),     nak
    end.



%%--------------------------------------------------------------------
-spec get_run_hash(Key :: string()) -> none
                                     | string().
%%
% @doc  Returns the results hash from a previous run, or `none' if
%       there is no recorded run for the specified key.
%
%       TODO: Later, we could have a better implementation with DETS.
%             However, we are expecting a good bit of by-hand tinkering
%             during this initial implementation.
% @end  --
get_run_hash(Key) ->
    RunResults = #{% TOP N RUNS:
                   %
                   % [{run,1},gw,{top_n,5,25},level,oter,fear]:
                   "D61A164696BDDDAA50CCEA917D9B19F984889A5489B47DC1E3F2FE40C81CF64C" =>
                   "2ECC755C6BB3F5E4D384364AE0FC65B284F2BF433C803FD32E4099549B86B4",    % Oct 2017 -- Jun 2018 T
                  %"1E5D27DE436A96BF5BE1C1B7513C2D196AF75CC4BC88678F8F6F95E17CB3C6",    % Oct 2017 -- Jun 2018 T PREV!
                  %"B9256B9D997E9F9FA8DE95A3615833A2F1EB061B01EE55F7DA50A5D09D4F32",    % Oct 2017 -- Jun 2018 CV
                  %"F3D06C823253176CB6E7CD31AF35BF7534E82DCB931412A27A45DF85A9C6",      % Oct 2017 -- Jun 2018 P
                  % ----------------------------------------------------------------    % -------------------- --
                  %"B877D1A57904AD967A8AA3A5774B18C4D276FD6ABBF2A1A56ED4D72CFC28D7C",   % Oct 2017 -- Mar 2018 CV

                   % --------------------------------------------------------------------------------------------
                   % [gw,{top_n,5,25},variation,oter,fear]:
                   "8A69D3DEA8771FC29DE45C2C4F51D97E5E1CA679E70581AA6882E39B05D79CF" =>
                   "349D3D34F24861ED24C5E7875173A1C1A1E17F07EB178F21668E49051825F0",    % Oct 2017 -- Jun 2018 T
                  %%1BFCA2360158A88FB45CF6BCCD5D310D43ADD371A9BB9A259640789C9EE066",    % Oct 2017 -- Jun 2018 CV
                  % ----------------------------------------------------------------    % -------------------- --
                  %%E54282667A2485BDEC43E8641523E8EAEDA0AD442373CCF1CD7F25E22CAF",      % Oct 2017 -- Mar 2018 T

                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{top_n,5,25},level,oter,anger]:
                   "AE727F720DC172FE4BBD63AC7C84486B980B6FF98F5FBC1A1790A0BFCE43" =>
                   "92AB3A2DE53B3D77DE92C5A2B49D5A93297CB6852B92CB11A1EFDB3124C8313",   % 2018-01-01__2018-10-01 T

                   % [{run,2},gw,{top_n,5,25},level,oter,fear]:
                   "A437A83557F7D366B99B08D3F21D2BF156ECEF24F99E9CB7BF58F3D316DC058" =>
                   "3B19F02B1BA0B221EBDC7CEFE3F37F7211A8E65FE92F7A47B463A9F47F120",     % 2018-01-01__2018-10-01 T

                   % [{run,2},gw,{top_n,5,25},level,oter,sadness]:
                   "6B3483AA23EE5DED56CC7CC135723E27DC98C3B56357FA848C53AED09E3F" =>
                   "FA3A463A60D2113FFE38AC162BF4F0C2BEC742746FB867830368CA7B476EF2",    % 2018-01-01__2018-10-01 T

                   % [{run,2},gw,{top_n,5,25},level,oter,joy]:
                   "5CA7052114ED28E2EE37247EFB63FE78BA892A4295641612B8CF8DDB8BAB" =>
                   "DBA15EA46B7A5D62D6271C7C8693924FB878FD648FB2CA7D7972DD5885CC4AA",   % 2018-01-01__2018-10-01 T

                   % -------------------------------------------------------------------------------------------
                   % [{run,2},gw,{top_n,5,25},variation,oter,anger]:
                   "FIXME" =>
                   "B5D010FE5C4FF8BA0B1B7AB8A5B3B396723EC772A9ED7D15CD014381EB4EDE7",   % Sep 2018 -- Aug 2019 T

                   % [{run,2},gw,{top_n,5,25},variation,oter,fear]:
                   "42115A234D67D1E668535FC402F6FAA7E6CA2424AF817678D5F637F12851D96" =>
                   "74FDC59D6B9698067CCEB68078BB947D32DB6B4F739F4C6FBE7BBE38DC172A",    % 2018-01-01__2018-10-01 T

                   % --------------------------------------------------------------------------------------------
                   % TOP N RUNS
                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,anger]:
                   "AB478199E4BCF1D1091D4C6B2EF7C57563C416455F2BA7039CE54FC4D48C7B" =>
                   "B919945D066F32C5EED6427957D223BCA13FC13340AEB22FDC6DA35692E974",    % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,fear]:
                   "51A6127FB9651C6C63198F34C67394F8B93409B8268E38B7ED78B26A8FD8D31" =>
                   "3B19F02B1BA0B221EBDC7CEFE3F37F7211A8E65FE92F7A47B463A9F47F120",     % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,sadness]:
                   "FBA8E99E2D8476C4793D19E7AF9970E66909C7EA6D1E09D588733BA904045" =>
                   "FA3A463A60D2113FFE38AC162BF4F0C2BEC742746FB867830368CA7B476EF2",    % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,joy]:
                   "5746887088D53AF625F451A5D169D0BEA3C38C86F8CD91BC704A79D8AA66" =>
                   "DBA15EA46B7A5D62D6271C7C8693924FB878FD648FB2CA7D7972DD5885CC4AA"    % 2018-01-01__2018-10-01
                  },
    maps:get(Key, RunResults, none).

