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
         run_top_nn/2, run_top_nn/3]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(MIN_H0_TWEETS,  40).                % Low values make for empty rter|tmed days

-type dataset()      :: parms | train | test.
-type verification() :: ack | nak | undefined.


%%--------------------------------------------------------------------
-spec period(DataTag :: dataset()) -> proplist().
%%
%% Shortcut to get tracking run period.
%%
%% RUN-1: October 1, 2017 -- June 30, 2018              (10-fold CV)
%%--------------------------------------------------------------------
%eriod(parms) -> [{start, {2019, 01, 01}}, {stop, {2019, 04, 01}}];
period(parms) -> [{start, {2017, 09, 01}}, {stop, {2017, 12, 01}}]; % FIXME: overlap!
period(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 07, 01}}]; % FIXME!
%eriod(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 04, 01}}]; % FIXME! shortie
period(test)  -> [{start, {2018, 04, 01}}, {stop, {2018, 07, 01}}].
%eriod(test)  -> [{start, {2019, 10, 01}}, {stop, {2019, 12, 31}}].


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
        % Fold the usernames into a map indexed by non-negative integers for random retreival
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
-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy()) -> verification().

-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy(),
                 Options :: proplist()) -> verification().
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

    io:format("This will destroy the current raven and player states.~n"),
    case ioo:read_down("Are you sure? ") of
        "yes" ->
            case weka:ping(raven:get_jvm_node(Tracker)) of
                timeout -> [];
                _       -> run_top_nn_aux(Tracker, RunTag, Options)
            end;
        _ -> []
    end.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec run_top_nn_aux(Tracker :: tracker(),
                     RunTag  :: stringy(),
                     Options :: proplist()) -> verification().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
% @end  --
run_top_nn_aux(Tracker, RunTag, Options) ->

    % Function to pull and organize players and their tweets for a given dataset
    GetPlayers = fun(D) ->
        sila:reset(),
        ok = raven:emote(Tracker, period(D)),
        wait_on_players(Tracker),
        player:get_players(Tracker)
    end,

    % The training dataset gives the player community that deines the Top-N groups
    Players = GetPlayers(train),
    Method  = {top_n, Min, Max} = influence:init_range(top_n),
    TopBiggies = maps:from_list([{N, player:get_top_n(Tracker, N)} || N <- lists:seq(Min, Max)]),
    TopMediums = maps:map(fun(_, Bigs) -> make_h0(Bigs, Players) end,
                          TopBiggies),

    % We need player communities for the other datasets as well.  "Ref" here means "null hypothesis".
    DataMode = proplists:get_value(data_mode, Options, level),
    DataSets = maps:from_list([{train, Players} | [{D, GetPlayers(D)} || D <- [parms, test]]]),
    RunOpts  = [{datasets, DataSets}, {toppers,  TopBiggies} | Options],
    RefOpts  = [{datasets, DataSets}, {toppers,  TopMediums} | Options],
    RefTag   = ?str_fmt("~s.H0", [RunTag]),

    Totals = #{tter := Count} = wait_on_players(Tracker),
    ?notice("Completed processing ~p tweets", [Count]),
    maps:map(fun(Comm, Cnt) ->
                 ?info("* ~s: ~B", [Comm, Cnt]) end,
             Totals),

    % Create the models
    RunResults = influence:run_top_nn(Tracker, RunTag, oter, fear, RunOpts),
    RefResults = influence:run_top_nn(Tracker, RefTag, oter, fear, RefOpts),

    Report = fun
        Recur([], []) ->
            ok;
        Recur([{N, {RunPCC, RunAttrs}} | RunRest],
              [{N, {RefVal, RefData}}  | RefRest]) ->
            % We may not have had enough data for a reference score
            {RefPCC,
             ModelInfo} = case RefVal of
                R when is_float(R)  -> {?str_fmt("~7.4f", [R]), RunAttrs};
                need_data           -> {"*******",              RefData}
            end,
            ?info("N @ ~2B: pcc[~7.4f] ref[~s] model~p",
                  [N, RunPCC, RefPCC, ModelInfo]),
            Recur(RunRest, RefRest)
    end,
    Report(RunResults, RefResults),
    verify_run(Tracker, RunTag, Method, DataMode,  oter, fear, RunResults).



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
                 RunTag   :: stringy(),
                 Method   :: tuple(),
                 DataMode :: data_mode(),
                 Comm     :: comm_code(),
                 Emo      :: emotion(),
                 Results  :: proplist()) -> verification().
%%
% @doc  Generates a hash representing the results of a given run.
%       If we know what the results should be (we have a hash from
%       a previous run), then we compare the current run's results
%       and warn the user they don't match up.
% @end  --
verify_run(Tracker, RunTag, Method, DataMode, Comm, Emo, Results) ->

    % Function to create a hexstring SHA-256 fingerprint
    Hash = fun(Elm) ->
        lists:flatten([io_lib:format("~.16B", [X]) || <<X>> <= crypto:hash(sha256, term_to_binary(Elm))])
    end,

    Cfg = [Tracker, Method, DataMode, Comm, Emo],
    Key = Hash(Cfg),
    Val = Hash(Results),

    ?info("RUN: ~s % ~p: ~p => ~p", [RunTag, Cfg, Key, Val]),
    case get_run_hash(Key) of
        none -> ?notice("No previous results to compare"),                          undefined;
        Val  -> ?notice("Results are consistent!"),                                 ack;
        Prev -> ?warning("Results do not match previous runs: prev[~p]", [Prev]),   nak
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
    RunResults = #{% [gw,{top_n,5,25},level,oter,fear]:
                   "352931CCD47628D146746AB03F20697676A49FB32998DBBD28FD568CDEF9A9DB" =>
                   "B9256B9D997E9F9FA8DE95A3615833A2F1EB061B01EE55F7DA50A5D09D4F32",  % 2017-10-01--2018-07-01 CV
                  %"F3D06C823253176CB6E7CD31AF35BF7534E82DCB931412A27A45DF85A9C6",    % 2017-10-01--2018-07-01 Prm
                  %"1E5D27DE436A96BF5BE1C1B7513C2D196AF75CC4BC88678F8F6F95E17CB3C6",  % 2017-10-01--2018-07-01 Tst
                  %"B877D1A57904AD967A8AA3A5774B18C4D276FD6ABBF2A1A56ED4D72CFC28D7C", % 2017-10-01--2018-04-01 CV

                  % [gw,{top_n,5,25},variation,oter,fear]:
                  "8A69D3DEA8771FC29DE45C2C4F51D97E5E1CA679E70581AA6882E39B05D79CF" =>
                  "E54282667A2485BDEC43E8641523E8EAEDA0AD442373CCF1CD7F25E22CAF",     % 2017-10-01--2018-07-01 T

                   % [gw,{top_n,10,25},oter,fear]: [TODO]
                   "C9D3FC24B8A4CC261E436434E7AA6CCCF6855A8D27554784ECA9EF9D41FC9374" =>
                   "62FD21C13BC2EF8891B5B8E9BA88817D42411862FFE8464B64C6FC26E1224BB"
                  },
    maps:get(Key, RunResults, none).

