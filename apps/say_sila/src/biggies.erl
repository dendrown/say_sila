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

-export([go/0,          go/1,               % Shortcut/convenience functions
         period/1]).
-export([clear_cache/0,
         make_h0/2,
         run_top_n/2,  run_top_n/3,
         run_top_nn/2, run_top_nn/3]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("llog/include/llog.hrl").

-define(MIN_H0_TWEETS,  40).                % Low values make for empty rter|tmed days
-define(NUM_H0_RUNS,    10).                % Number of H0 runs to average together
%define(NUM_H0_RUNS,     2).                % Number of H0 runs to average together
-define(FPATH_RNG_SEED, ?WORK_DIR "/influence/rand.seed.etf").
-define(RUNS_CACHE,     ?WORK_DIR "/dets/biggies_runs").
-define(MEASURES,       [correlation, error_mae, error_rmse]).

-type dataset()          :: parms | train | test.
-type dataset_prop()     :: {dataset(), term()}.
-type dataset_proplist() :: [dataset_prop()].
-type verification()     :: ack | nak | undefined.
-type verifications()    :: [verification()].

-type run_code() :: n | nn.
-type run_fun()  :: fun((tracker(), stringy(), comm_code(), emotion(), proplist()) -> proplist()).
-type method()   :: {run_code(), {atom(), pos_integer(), pos_integer()}}.



%%--------------------------------------------------------------------
go() ->
    go(n).


go(Method) ->
    % Ref: http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf
    rand:uniform(),
    Seed = rand:export_seed(),
    ?debug("Random seed: ~p", [Seed]),
    file:write_file(?FPATH_RNG_SEED, term_to_binary(Seed)),
    %rand:seed_s(exsss),

    % Make it so...!
    Go = maps:get(Method, #{n  => fun run_top_n/3,
                            nn => fun run_top_nn/3}),
    Go(gw, lregv12r25, [{data_mode, variation}, {sweep, 9}]).



%%--------------------------------------------------------------------
-spec period(DataTag :: dataset() | p100) -> proplist()
                                           | {p100, float()}.
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

period(parms_pct) -> {p100, 0.25};     % <<= Specify in Options
period(parms) -> [{start, {2017, 10, 01}}, {stop, {2018, 01, 01}}];
%eriod(train) -> [{start, {2018, 01, 01}}, {stop, {2018, 10, 01}}];
%eriod(test)  -> [{start, {2018, 10, 01}}, {stop, {2019, 01, 01}}].
%%--------------------------------------------------------------------
%% CV : 9 periods of 12 months
%%--------------------------------------------------------------------
%eriod(train) -> [{start, {2018, 01, 01}}, {stop, {2019, 01, 01}}];
%eriod(test)  -> [{start, {2019, 01, 01}}, {stop, {2019, 04, 01}}].
%%--------------------------------------------------------------------
%% Random sample test : 9 periods of 12 months
%%--------------------------------------------------------------------
period(train) -> [{start, {2018, 01, 01}}, {stop, {2019, 01, 01}}];
period(test)  -> {p100, 0.25}.

-else.
%%--------------------------------------------------------------------
%% RUN-1: October 1, 2017 -- June 30, 2018              (10-fold CV)
%%--------------------------------------------------------------------
%eriod(parms) -> [{start, {2019, 01, 01}}, {stop, {2019, 04, 01}}];
period(parms) -> [{start, {2017, 09, 01}}, {stop, {2017, 12, 01}}]; % FIXME: overlap!
period(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 07, 01}}];
%eriod(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 04, 01}}]; % FIXME! shortie
period(test)  -> [{start, {2018, 04, 01}}, {stop, {2018, 07, 01}}].
%eriod(test)  -> [{start, {2019, 10, 01}}, {stop, {2019, 12, 31}}].
-endif.


%%--------------------------------------------------------------------
-spec clear_cache() -> ok
                     | {error, term()}.
%%
% @doc  Clear development cache.
% @end  --
clear_cache() ->
    dets:delete_all_objects(?RUNS_CACHE).



%%--------------------------------------------------------------------
-spec make_h0(Biggies :: proplist(),
              Players :: map()) -> proplist().
%%
% @doc  Selects a set of null hypothesis (H0) "medium" players for
%       comparison runs.
% @end  --
make_h0(Biggies, Players) ->
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
        ?info("Sampled ~2B medium (H0) ~s players from ~B with at least ~B tweets",
              [length(Meds), Comm, MediumCnt, ?MIN_H0_TWEETS]),
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
%                           `m5rules' for M5Rules.
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
-spec prep_data(Tracker :: tracker(),
                Method  :: method(),
                Periods :: dataset_proplist(),
                Options :: proplist()) -> {map(),
                                           map(),
                                           [{pos_integer(), map()}]}.
%%
% @doc  Prepares the `train', `parms' and  `test' datasets as well as
%       the Big and several Medium (null hypothesis) player communities.
% @end  --
prep_data(Tracker, Method, Periods, _Options) ->

    % The Method tuple gives us our data range
    {_, {top_n, Min, Max}} = Method,

    % Function to pull and organize players and their tweets for a given dataset
    GetPlayers = fun(Step) ->
        Period = proplists:get_value(Step, Periods),
        sila:reset(),
        case player:load(Tracker, Period) of
            none ->
                % Process tweets, rank players and save the results for next time
                ok = raven:emote(Tracker, Period),
                wait_on_players(Tracker),
                player:save(Tracker, Period);
            Info ->
                ?notice("Loaded players: ~p", [Info])
        end,
        player:get_players(Tracker)
    end,
    Players = GetPlayers(train),

    % The training dataset gives the player community that deines the Top-N groups
    TopBiggies  = maps:from_list([{N, player:get_top_n(Tracker, N)} || N <- lists:seq(Min, Max)]),
    TopMediumss = [{I, maps:map(fun(_, Bigs) -> make_h0(Bigs, Players) end, TopBiggies)}
                   || I <- lists:seq(1, ?NUM_H0_RUNS)],

    % The `p100' option specifies the percentage of training data to reserve
    % for parameter optimization.  0% means we should use an independent dataset.
    GetData = fun(Step) ->
        case proplists:get_value(Step, Periods) of
            {p100, P100} -> P100;
            _            -> GetPlayers(Step)
        end
    end,

    % The `test' dataset player community is always independent
    DataSets = maps:from_list([{train, Players}
                               | [{Step, GetData(Step)} || Step <- [parms, test]]]),
    {DataSets, TopBiggies, TopMediumss}.



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

    ioo:make_fpath(?RUNS_CACHE),
    dets:open_file(?RUNS_CACHE, [{repair, true}, {auto_save, 60000}]),

    io:format("This will destroy the current raven and player states.~n"),
    Return = case ioo:read_down("Are you sure? ") of
        "yes" ->
            case weka:ping(raven:get_jvm_node(Tracker)) of
                timeout -> [];
                _       -> do_run_run(RunCode, Tracker, RunTag, Options)
            end;
        _ -> []
    end,
    dets:close(?RUNS_CACHE),
    Return.


%%--------------------------------------------------------------------
-spec do_run_run(RunCode :: run_code(),
                 Tracker :: tracker(),
                 RunTag  :: stringy(),
                 Options :: proplist()) -> ok.

-spec do_run_run(RunFun  :: run_fun(),
                 Tracker :: tracker(),
                 Method  :: method(),
                 RunTag  :: stringy(),
                 RunNum  :: non_neg_integer(),
                 Periods :: dataset_proplist(),
                 Options :: proplist()) -> ref_run_proplist().

-type ref_run_proplist() :: {emotion_proplist(),
                             multi_run_average()}.
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
% @end  --
do_run_run(RunCode, Tracker, RunTag, Options) ->

    Method = {RunCode, {top_n, Min, Max} = influence:init_range(top_n)},

    % We'll be calling into one of the `influence' run functions
    RunFun = case RunCode of
        n  -> fun influence:run_top_n/5;
        nn -> fun influence:run_top_nn/5
    end,

    % The `period' function defines the start of our sweep (if any)
    BasePeriodSet = [{DS, period(DS)} || DS <- [parms, train, test]],
    MovePeriod = fun(M, Period) ->
        case {M, Period} of
            {0, _}          -> Period;
            {_, {p100, _}}  -> Period;
            _ ->
                [{BegEnd, dts:add(DTS, M, month)} || {BegEnd, DTS} <- Period]
        end
    end,
    RunPeriodSet = fun(M) ->
        PeriodSet = [{DS, MovePeriod(M, Per)} || {DS,Per} <- BasePeriodSet],
        do_run_run(RunFun, Tracker, Method, RunTag, M, PeriodSet, Options)
    end,

    % Are we sweeping a period window a month at a time?
    case proplists:get_value(sweep, Options, 1) of
        1 ->
            RunPeriodSet(0),
            ?notice("End of single-period run");

        S when S > 1 ->
            % NOTE: the RefResultsSets are running averages of the H0 runs in a period.
            RunRefResultsSet = [{M, RunPeriodSet(M-1)} || M <- lists:seq(1, S)],

            % TODO: Make `average_results' do this automatically
            RunResultsSet = [{M, Run} || {M,{Run,_}} <- RunRefResultsSet],
            RefResultsSet = [{M, Ref} || {M,{_,Ref}} <- RunRefResultsSet],
            ?debug("Run results set: ~p", [RunResultsSet]),
            ?debug("Ref results set: ~p", [RefResultsSet]),

            AvgRunResults = average_results(RunResultsSet),
            AvgRefResults = average_results(RefResultsSet),
            ?debug("Run results avg: ~p", [AvgRunResults]),
            ?debug("Ref results avg: ~p", [AvgRefResults]),

            % The model we display corresponds to the "best N" based on the averaged results
            RunResults = averages_to_results(AvgRunResults),
            RefResults = averages_to_results(AvgRefResults),
            BestRun = find_best_run(AvgRunResults, RunResultsSet),

            % For reporting, we'll show the full period sweep
            SweepPeriod = fun(Period) ->
                case Period of
                        % Sampling  Percentage
                    {p100,_}
                        -> Period;
                    _ ->
                        {DTS, Rest} = pprops:get_split(stop, Period),
                        [{stop, dts:add(DTS, S, month)} | Rest]
                end
            end,
            RptPeriodSet = [{DS, SweepPeriod(Per)} || {DS,Per} <- BasePeriodSet],

            report(Tracker, Method, roll_up, RptPeriodSet, Options, RunResults, RefResults),
            ?notice("BEST: ~p", [BestRun])
    end.


do_run_run(RunFun, Tracker, Method, RunTag, RunNum, PeriodSet, Options) ->

    % We need Big and Medium (H0) player communities.  "Ref" here means "null hypothesis".
    {DataSets,
     TopBiggies,
     TopMediumss} = prep_data(Tracker, Method, PeriodSet, Options),

    PreOpts  = [{datasets, DataSets} | Options],                        % Shared by run & refs
    RunOpts  = [{toppers,  TopBiggies} | PreOpts],
    RefOptss = [{I, [{toppers, TMs} | PreOpts]} || {I,TMs} <- TopMediumss],

    Totals = #{tter := Count} = wait_on_players(Tracker),
    ?notice("Completed processing ~p tweets", [Count]),
    maps:map(fun(Comm, Cnt) ->
                 ?info("* ~s: ~B", [Comm, Cnt]) end,
             Totals),

    % Functions to create and run models for all emotions
    RunTagNum = case RunNum of
        0 -> RunTag;
        _ -> ?str_fmt("~s+~B", [RunTag, RunNum])
    end,
    Process = fun(Tag, Opts) ->
        [{Emo, RunFun(Tracker, Tag, oter, Emo, Opts)} || Emo <- ?EMOTIONS]
    end,

    ProcessH0 = fun(I, Opts) ->
        RefTag = ?str_fmt("~s_H0_~B", [RunTagNum, I]),
        Process(RefTag, Opts)
    end,

    % Process sets of run and reference models
    RunResults  = Process(RunTagNum, RunOpts),
    RefResultss = [{I, ProcessH0(I, Opts)} || {I,Opts} <- RefOptss],

    %?debug("RefResultss:~n~p", [RefResultss]),
    AvgRefResults = average_results(RefResultss),

    %?info("RefResults:~n~p", [RefAvgResults]),
    RefResults = averages_to_results(AvgRefResults),

    report(Tracker, Method, RunNum, PeriodSet, Options, RunResults, RefResults),
    {RunResults, AvgRefResults}.



%%--------------------------------------------------------------------
-spec average_results(Results :: multi_run_results()) -> multi_run_average().

-spec average_results(Results :: multi_run_results()
                               | multi_run_average(),
                      Acc     :: multi_run_average()) -> multi_run_average().

-type multi_run_result()  :: {pos_integer(), emotion_proplist()}.
-type multi_run_results() :: [multi_run_result()].
-type multi_run_average() :: #{emotion() := #{pos_integer() := map()}}.
%%
% @doc  Combines and averages a list of reference runs.
%
%       Example input:
%        ```
%       [{1,[{anger,[{5,{need_data,#{oter => 1.0,rted => 0.32,rter => 0.83,tmed => 0.83}}},
%                    ...
%                    {22,#{correlation => 0.0403, error_mae => 0.0173, error_rmse => 0.0209}, ...]
%            {fear, [...]} ...]
%        {2,[...]}
%        ...]
%       '''
%
%        Output template:
%        ```
%        #{anger := #{N := #{good_cnt  := GC,
%                            fail_cnt  := FC,
%                            pcc       := PCC,
%                            need_data := ND}}}
%       '''
%
%       TODO: [{I, Runs, Refs} | ...]
% @end  --
average_results(Results) ->
    ?info("Averaging ~B run results.", [length(Results)]),
    average_results(Results, #{}).


average_results([], Acc) ->
    Acc;

average_results([{_, Results}|Rest], Acc) when is_map(Results) ->
    % Here we are merging in a previous set of averaged results
    %
    % Functions to merge the N-maps for two avg-result-maps
    MergeN = fun(N, Ns = [RunNs, AccNs]) ->
        Averages = [RunAvg,
                    AccAvg] = [maps:get(N, Avgs, #{}) || Avgs <- Ns],

        % Function to retrieve a good|fail count from both maps
        ReCount = fun(CntType) ->
            [maps:get(CntType, Avg, 0) || Avg <- Averages]
        end,

        % Make sure we have at least one good run
        NewGoodAvg = case ReCount(good_cnt) of
            [0, 0] -> #{};

            [RunGoodCnt, AccGoodCnt] ->
                % We're working with a few measures for good models
                RunGoodAverage = fun(X) ->
                    {X, average(maps:get(X, RunAvg, 0.0), RunGoodCnt,
                                maps:get(X, AccAvg, 0.0), AccGoodCnt)}
                end,

                GoodMeasures = [RunGoodAverage(X) || X <- ?MEASURES],
                GetGoodness  = fun(X, Elm) ->
                    element(case Elm of
                                measure -> 1;
                                count   -> 2
                            end,
                            proplists:get_value(X, GoodMeasures))
                end,
                #{good_cnt    => GetGoodness(correlation, count),
                  correlation => GetGoodness(correlation, measure),
                  error_mae   => GetGoodness(error_mae,   measure),
                  error_rmse  => GetGoodness(error_rmse,  measure)}
        end,

        % Also, do we have at least one model that didn't make it...?
        NewFailAvg = case ReCount(fail_cnt) of
            [0, 0] -> #{};

            [RunFailCnt, AccFailCnt] ->
                {NewNeeds,
                 NewFailCnt} = average_needs(maps:get(need_data, RunAvg, #{}), RunFailCnt,
                                             maps:get(need_data, AccAvg, #{}), AccFailCnt),
                #{fail_cnt  => NewFailCnt,
                  need_data => NewNeeds}
        end,
        NewAvg = maps:merge(NewGoodAvg, NewFailAvg),
        [RunNs, maps:put(N, NewAvg, AccNs)]
    end,

    MergeEmo = fun(Emo, EmoAcc) ->
        RunNs = maps:get(Emo, Results, #{}),
        AccNs = maps:get(Emo, EmoAcc,  #{}),
        [_,
         NewNs] = lists:foldl(MergeN, [RunNs,AccNs], maps:keys(RunNs)),
        maps:put(Emo, NewNs, EmoAcc)
    end,
    average_results(Rest, lists:foldl(MergeEmo, Acc, ?EMOTIONS));


average_results([{_, Results}|Rest], Acc) ->

    % Function to act on all Top-N values for a single emotion model
    Topper = fun({N, Info}, TopAcc) ->
        NAcc = maps:get(N, TopAcc, #{}),
        NewNAcc = case Info of
            % Keep a running average of failure scores
            {need_data, CommCats} ->
                {NewNeeds,
                 NewCnt} = average_needs(CommCats,
                                         maps:get(need_data, NAcc, #{}),
                                         maps:get(fail_cnt,  NAcc, 0)),
                maps:merge(NAcc, #{need_data => NewNeeds,
                                   fail_cnt  => NewCnt});

            % And a running average of good scores
            _ ->
                % Add one good model to the running average for each measure X
                GoodCnt = maps:get(good_cnt, NAcc, 0),
                RunGoodAverage = fun(X, Val) ->
                    {NewAvg, _} = average(Val, maps:get(X, NAcc, 0.0), GoodCnt),
                    NewAvg
                end,
                Measures = maps:with(?MEASURES, Info),
                maps:merge(maps:put(good_cnt, GoodCnt+1, NAcc),
                           maps:map(RunGoodAverage, Measures))
        end,
        maps:put(N, NewNAcc, TopAcc)
    end,

    % Function to act on all the emotions in a single run
    Emoter = fun({Emo, EmoResultsByN}, EmoAcc) ->
        TopAcc = maps:get(Emo, EmoAcc, #{}),
        NewTopAcc = lists:foldl(Topper, TopAcc, EmoResultsByN),
        maps:put(Emo, NewTopAcc, EmoAcc)
    end,

    % Average in the current run and recurse for the remaining runs
    average_results(Rest, lists:foldl(Emoter, Acc, Results)).



%%--------------------------------------------------------------------
-spec averages_to_results(AvgResults :: multi_run_average()) -> list().
%%
% @doc  Creates a single result set from  a list of averaged results.
%
%       Input:
%        ```
%        #{anger := #{N := #{good_cnt    := GC,
%                            fail_cnt    := FC,
%                            correlation := PCC,
%                            need_data   := ND}}}
%       '''
%
%        Output:
%        ```
%       [{anger,[{5,{need_data,#{oter => 1.0,rted => 0.32,rter => 0.83,tmed => 0.83}}},
%                ...
%                {22,#{correlation => 0.0403, error_mae => 0.0173, error_rmse => 0.0209}, ...]
%        {fear, [...]} ...]
%       '''
% @end  --
averages_to_results(AvgResults) ->

    % FIXME: Make the good/bad roll_up results match run results: {code, map}
    %
    % Function to associate the average successful (or alterately failed) model to a given N
    Topper = fun(N, Averages) ->
        AvgValue = case Averages of
            #{good_cnt  := Cnt}     when Cnt > 0  -> Averages;

            #{fail_cnt  := Cnt,
              need_data := Needs}   when Cnt > 0  -> {need_data, Needs}
        end,
        {N, AvgValue}
    end,

    % Function to convert averaged results to the "standard" format for the specified emotion
    Emoter = fun(Emo) ->
        TopMap = maps:get(Emo, AvgResults),
        TopLst = maps:to_list(TopMap),
        [Topper(N, Avgs) || {N,Avgs} <- lists:sort(TopLst)]
    end,

    % Format the averages as standard results
    [{Emo, Emoter(Emo)} || Emo <- ?EMOTIONS].



%%--------------------------------------------------------------------
-spec average(NewVal :: number(),
              OldAvg :: float(),
              OldCnt :: non_neg_integer()) -> average_count().

-spec average(NewVal :: number(),
              NewCnt :: pos_integer(),
              OldAvg :: float(),
              OldCnt :: non_neg_integer()) -> average_count().

-type average_count() :: {float(), pos_integer()}.
%%
% @doc  Computes a new running average.
% @end  --
average(NewVal, OldAvg, OldCnt) ->
    average(NewVal, 1, OldAvg, OldCnt).


average(NewVal, NewCnt, OldAvg, OldCnt) ->
    NewAvgCnt = NewCnt + OldCnt,
    {(NewCnt*NewVal + OldCnt*OldAvg) / NewAvgCnt, NewAvgCnt}.


%%--------------------------------------------------------------------
-spec average_needs(NewNeeds :: map(),
                    OldNeeds :: map(),
                    OldCount :: non_neg_integer()) -> need_data_count().

-spec average_needs(NewNeeds :: map(),
                    NewCount :: pos_integer(),
                    OldNeeds :: map(),
                    OldCount :: non_neg_integer()) -> need_data_count().

-type need_data_count() :: {map(), pos_integer()}.
%%
% @doc  Computes a new running average for a `need_data' map.
% @end  --
average_needs(NewNeeds, OldNeeds, OldCount) ->
    average_needs(NewNeeds, 1, OldNeeds, OldCount).


average_needs(NewNeeds, NewCount, OldNeeds, OldCount) ->

    Average = fun(Comm, OldPct) ->
        {Avg,_} = average(maps:get(Comm, OldNeeds, 0.0), NewCount, OldPct, OldCount),
        Avg
    end,

    case {maps:size(OldNeeds), OldCount} of
        {0,0} -> {NewNeeds, NewCount};
        _     -> {maps:map(Average, NewNeeds), NewCount+OldCount}
    end.



%%--------------------------------------------------------------------
-spec find_best_run(AvgResults :: multi_run_average(),
                    ResultsSet :: multi_run_results()) -> #{emotion() := tuple()}.
%%
% @doc  Uses the AvgResults to find the best N across emotions and across
%       a set of model results.
% @end  --
find_best_run(AvgResults, ResultsSet) ->

    % Function to select the best N for a given emotion using the average across runs
    FindBestEmoN = fun
        (N, #{correlation := PCC}, Best = {_, BestPCC}) ->
            case PCC > BestPCC of
                false -> Best;
                true  -> {N, PCC}
            end;

        (_, _, Best) -> Best
    end,

    % Function to fold over emotions, selecting the best N for each
    FindBestEmoNs = fun(Emo, Avgs, Acc) ->
        {BestEmoN,_} = maps:fold(FindBestEmoN, {0, -1.0}, Avgs),
        [{Emo, BestEmoN} | Acc]
    end,
    BestEmoNs = maps:fold(FindBestEmoNs, [], AvgResults),
    BestN = round(lists:foldl(fun({_,N}, Acc) -> Acc+N end, 0, BestEmoNs) / length(BestEmoNs)),

    ?notice("Using N=~B: best~p", [BestN, BestEmoNs]),

    % Reduce to find the run that has the best score for an emotion for our chosen value of N
    FindBestEmoRun = fun({Emo, ResultsByN}, Acc = {I, EmoAcc}) ->

        PCC = maps:get(correlation,
                       proplists:get_value(BestN, ResultsByN, #{}),
                       -1.0),
        {_,_,
         BestPCC} = maps:get(Emo, EmoAcc, {0, BestN, -1.0}),

        case PCC > BestPCC of
            false -> Acc;
            true  -> {I, maps:put(Emo, {I, BestN, PCC}, EmoAcc)}
        end
    end,

    % Reduce to find the best run for all emotions for our chosen value of N
    FindBestRuns = fun({I, RunInfo}, Acc) ->
        {_,BestEmoRuns} = lists:foldl(FindBestEmoRun, {I, Acc}, RunInfo),
        BestEmoRuns
    end,

    lists:foldl(FindBestRuns, #{}, ResultsSet).


%%--------------------------------------------------------------------
-spec report(Tracker    :: tracker(),
             Method     :: tuple(),
             RunNum     :: non_neg_integer() | roll_up,
             PeriodSet  :: dataset_proplist(),
             Options    :: proplist(),
             RunResults :: proplist(),
             RefResults :: proplist()) -> verifications().
%%
% @doc  Report and compare the biggie results against the averaged reference results
% @end  --
report(Tracker, Method, RunNum, PeriodSet, Options, RunResults, RefResults) ->

    Report = fun
        Recur([], [], Verifications) ->
            Verifications;
        Recur([{Emo, Run} | RestRuns],
              [{Emo, Ref} | RestRefs], Verifications) ->
            V = report_run(Tracker, Method, RunNum, Emo, PeriodSet, Options, Run, Ref),
            Recur(RestRuns, RestRefs, [V|Verifications])
    end,
    Report(RunResults, RefResults, []).



%%--------------------------------------------------------------------
-spec report_run(Tracker    :: tracker(),
                 Method     :: tuple(),
                 RunNum     :: non_neg_integer() | roll_up,
                 Emotion    :: emotion(),
                 PeriodSet  :: dataset_proplist(),
                 Options    :: proplist(),
                 RunResults :: proplist(),
                 RefResults :: proplist()) -> verification().
%%
% @doc  Reports the results of one run (with reference) for one emotion.
% @end  --
report_run(Tracker, Method, RunNum, Emotion, PeriodSet, Options, RunResults, RefResults) ->

    DataMode = proplists:get_value(data_mode, Options, level),

    % Is this a multi-month sweep?  We'll get M = 0, 1, ..., roll_up
    SweepTxt = case {RunNum, proplists:get_value(sweep, Options, fixed)}  of
        {0, fixed} -> <<>>;
        {M, Sweep} ->
            RunTxt = case M of
                roll_up -> <<"Final results">>;
                _       -> ?str_fmt("~B", [M+1])
            end,
            ?str_fmt(" (~s of ~B month sweep)", [RunTxt, Sweep])
    end,

    % Announce the report and log the time periods
    ?notice("Reporting '~s' run for ~s~s", [DataMode, Emotion, SweepTxt]),
    LogStamper = fun(Step) ->
        Period = case proplists:get_value(Step, PeriodSet) of
            {_,P100} when is_float(P100) -> ?str_fmt("~.1f%", [100 * P100]);
            DTSs     when is_list(DTSs)  -> ?str_fmt("~s__~s",
                                                     [dts:str(proplists:get_value(T, DTSs)) || T <- [start,stop]])
        end,
        ?info("Period ~-5s: ~s", [Step, Period])
    end,
    [LogStamper(Step) || Step <- [parms, train, test]],

    % Function to check a value and add a warning to a collection if it is not correct
    Measures = [pcc,mae,rmse],
    Checker  = fun
        (_, _, #{correlation := PCC,
                 error_mae   := MAE,
                 error_rmse  := RMSE}, Warnings) ->
            Scores = [?str_fmt("~s[~7.4f] ", [Tag, X]) || {Tag, X} <- lists:zip(Measures, [PCC,MAE,RMSE])],
            {lists:flatten(Scores), Warnings};

        (Step, N, {need_data, CommPcts}, Warnings) ->
            Scores = [?str_fmt("~s[*******] ", [Txt]) || Txt <- Measures],
            {lists:flatten(Scores), [{Step, N, CommPcts} | Warnings]}
    end,

    % The model description changes based on what happened and if it's a composite
    Describe = fun
        (#{incl_attrs := Attrs}) -> Attrs;                  % Linear regression
        (#{good_cnt := Cnt})     -> [valid, Cnt];           % Average across runs
        ({need_data, CommPcts})  -> CommPcts;               % All four comms not @ 100%
        (_)                      -> <<"??">>                % Black box
    end,

    % Report the results, collecting the warnings so we can log them all after the report
    Report = fun
        Recur([], [], Warnings) ->
            Warnings;
        Recur([{N, RunInfo} | RunRest],
              [{N, RefInfo} | RefRest], Warnings) ->
            % We may not have had enough data for a reference score
            {RunScores, MidWarnings} = Checker(run, N, RunInfo, Warnings),
            {RefScores, NewWarnings} = Checker(ref, N, RefInfo, MidWarnings),
            ?info("N @ ~2B: BIG< ~s> REF< ~s> model~p", [N, RunScores, RefScores, Describe(RunInfo)]),
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
                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,anger,{data_mode,level}]:
                   "DA7DE16D87C10844AAA148919CC356685F32A41B184B55215E4F1A3576A7AA8" =>
                   "92AB3A2DE53B3D77DE92C5A2B49D5A93297CB6852B92CB11A1EFDB3124C8313",   % 2018-01-01__2018-10-01 T

                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,fear,{data_mode,level}]:
                   "2A8312F129F0D7F637703EE2140B9B260092CE7E133AB048A067A963474C90" =>
                   "3B19F02B1BA0B221EBDC7CEFE3F37F7211A8E65FE92F7A47B463A9F47F120",     % 2018-01-01__2018-10-01 T

                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,sadness,{data_mode,level}]:
                   "5BE1D0C1E0CDFB62EDD59E30F2E023624AB4F41C8421CCE27E6B9473A32EFA12" =>
                   "FA3A463A60D2113FFE38AC162BF4F0C2BEC742746FB867830368CA7B476EF2",    % 2018-01-01__2018-10-01 T

                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,joy,{data_mode,level}]:
                   "8B52E11CBEFF92D741B461E829B62917E1B2FC662721C62A9B510F6545579F0" =>
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
                   "DBA15EA46B7A5D62D6271C7C8693924FB878FD648FB2CA7D7972DD5885CC4AA",   % 2018-01-01__2018-10-01

                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,anger,{data_mode,variation}]:
                   "31A65C246B78AC54430638257B51C8B513DBE84BED07F68E42F8E3862B39A" =>
                   "4D2DDD88B0CAF8CDC86C58F9FDA0717EFCC11189B949EA93DAD59ED6676CA44",   % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,fear,{data_mode,variation}]:
                   "661660D9C2B9ABCF186DA81C8EDD3E73D6CB6FD67E8731D4BAD37B434F1E2C2" =>
                   "90E3792C96FD953DB936C62148F7750D219B397B3FB921C92E5775D392734",     % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,sadness,{data_mode,variation}]:
                   "B72B105585D2E73A418C1EDB3A49CDEE1468BCEFD5AAD9ACBC3FC3BBD72E0" =>
                   "FDE59A421B142E5BE3D192FFF79481448964467E181BCD0E4E642674DAA5D13",   % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,joy,{data_mode,variation}]:
                   "31F3D57FE1647B11EE5D6C89AFA3091332410225A6C8D634DB892BFFBF5A3" =>
                   "786CDEAA1531289169967ACCF0614D59EFB0C193F781CBDD2DF195CCD36",       % 2018-01-01__2018-10-01

                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,anger,{learner,gproc}]:
                   "4442E9AD7076639B4AD1E81C049ECD5671F3257B4BCFE8A8675EBC82AB268" =>
                   "114475AE4B5A3FFC8FDCED624EAC2B81D36381616DDD0F37117244067E9B7",     % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,fear,{learner,gproc}]:
                   "99CAABC1F54C898C930B782FEF9DBEF951EC53BB183EBC77C65DA816784F6A" =>
                   "524747A5C2D45537E184748A27317FE158119BD7EAA8E2941F79419A781B",      % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,sadness,{learner,gproc}]:
                   "958B2FFEAC63A4559D25B1A36784313472D5E0DD19801156A9FDEF36C2F420" =>
                   "3B2431A725A11B8C837C2CC8B7C3FA459F3987B3665CDBA1056AEA3DB68B080",   % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},level,oter,joy,{learner,gproc}]:
                   "8231D5A3E93AAD54A2C86D52A6539B13FE473DA3D61D462A9693426EB599F5" =>
                   "EFA7BBDDD6D2CA7C73D6A5432163B1AC9BF8534DACE573441A00F1F1FE69",      % 2018-01-01__2018-10-01

                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,anger,{data_mode,variation},{learner,gproc}]:
                   "581D7E710BDCDCEC41E1F7F7D7C63F3A06448DCBA89FFEA32D4F83E11FA7" =>
                   "743BC97BFEFC2F9851CCC3CD5C9025FF80AAE8C3F65165135987271112AEF",     % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,fear,{data_mode,variation},{learner,gproc}]:
                   "EFEF476A45B19838BCDCCB87EDFAD3AE9015A3BAF670AAB27FA3EB9CB96B10" =>
                   "24118542D4B174734D28B9F196569E5CECDB579312E3A5404BE13D8624366716",  % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,sadness,{data_mode,variation},{learner,gproc}]:
                   "474BD355E37419A6767F18E93E5E4D6409A6D209760FD26D4EA2D4BC6892F5A" =>
                   "F55C828AC41C4F64EE33395EA79417E2E14764AEEDE0AE7B526F872ECE5AD5",    % 2018-01-01__2018-10-01

                   % [{run,2},gw,{n,{top_n,5,25}},variation,oter,joy,{data_mode,variation},{learner,gproc}]:
                   "19B4B9F182FAEFB083A9FF8569F3EB846DDEB8E574D9855832AF6FE86F66FE9D" =>
                   "35D0602934B67E11EF2DA512BD9A4545156D647909C6DAA68D951DDF3816280",   % 2018-01-01__2018-10-01

                   % --------------------------------------------------------------------------------------------
                   % Sample 25% for parms:
                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,anger,{parms_pct,0.25}]:
                   "21622789FA1F944C031CF54128630E4A53F2EAB63F01114D5693EE8707733AD" =>
                   "57E5993678D6E4BC78D593B0E4FB4BD7A2A8626DA8BC66EE9052C3D9AF7763",

                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,fear,{parms_pct,0.25}]:
                   "412E1D655CAF1B63B74C9778E8755448215AC562461126F8C13C12D9B6518F" =>
                   "3F03F433043E8A4F88BCB16F23A3B6131ADA7474E1B29E2750DA4A6CC9",        % 2017-10-01__2018-10-01

                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,sadness,{parms_pct,0.25}]:
                   "2CF16288FE5CE9D74E886B3414ADDE11E271BF78B9C5FEA169D8ABAC4B2E35DB" =>
                   "285D7E491C36C93F77152690E2C55D0E6F21A8BC4EAF01452E05263FEBA41",     % 2018-10-01__2019-01-01

                   % [{run,2},gw,{nn,{top_n,5,25}},level,oter,joy,{parms_pct,0.25}]:
                   "9626B849CF8A48D17C975CE0276BE3D81986D149F7D2B475812DC941CE4393BD" =>
                   "41350EDB56ECE63BC751F156A6EEB73C433A8DF472B8B11B08FD64FA552C0",     % 2018-10-01__2019-01-01

                   % --------------------------------------------------------------------------------------------
                   % [{run,2},gw,{nn,{top_n,5,25}},variation,oter,anger,{data_mode,variation},{parms_pct,0.25}]:
                   "494FBCACB0D0D265E89C6DEAB4617EF36E4F16C0976DACFE9CFCDEAF0877" =>
                   "8D69AC5696C28825ED18824FF93E213EDA167532F234312E145F82382CC3A39F",  % 2017-10-01__2018-10-01

                   % [{run,2},gw,{nn,{top_n,5,25}},variation,oter,fear,{data_mode,variation},{parms_pct,0.25}]:
                   "CBC4B3E07C534F5F63D3B3CBABB0EA2313B5F8595163D46C2C85CFBD2679FA7" =>
                   "82F2A58D723F90DA671655EEBC865EE5D8E51F299C2E6AA5D1FC976E7BB2F",     % 2017-10-01__2018-10-01

                   % [{run,2},gw,{nn,{top_n,5,25}},variation,oter,sadness,{data_mode,variation},{parms_pct,0.25}]:
                   "7EFD048CF62DF9DAC443FF099C54B50EBA3E7AEDCD2FFB3EC61FB12736D95B4" =>
                   "DD1DF84CE6FFE2FF2817A09A97325632833F5FD195249570377125F3452E2139",  % 2017-10-01__2018-10-01

                   % [{run,2},gw,{nn,{top_n,5,25}},variation,oter,joy,{data_mode,variation},{parms_pct,0.25}]:
                   "ACFCB2F977B72FA357869B89E4CE3A2260FB6BC02851884171421F9AA7AEE9A" =>
                   "DE7CBE8D7713732C7D914A102EAAFA478D6D87CD9E3F72277811BE771EB2562"    % 2017-10-01__2018-10-01
                  },
    maps:get(Key, RunResults, none).



%%====================================================================
%% Unit tests
%%--------------------------------------------------------------------
average_test() ->
    {10.0, 1} = average(10,  0.0, 0),
    { 5.0, 2} = average( 0, 10.0, 1),

    { 7.5, 4} = average( 5, 2, 10.0, 2).


%%--------------------------------------------------------------------
average_needs_test() ->
    Full = #{oter => 1.00, rter => 1.00, rted => 1.00, tmed => 1.00},
    Half = #{oter => 0.50, rter => 0.50, rted => 0.50, tmed => 0.50},
    X_75 = #{oter => 0.75, rter => 0.75, rted => 0.75, tmed => 0.75},

    {Full, 1} = average_needs(Full, #{},  0),
    {X_75, 2} = average_needs(Half, Full, 1),

    {X_75, 4} = average_needs(Half, 2, Full, 2).

