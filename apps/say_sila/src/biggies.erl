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

-export([period/1,
         run_top_nn/2]).

-include("sila.hrl").
-include("emo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-type dataset() :: parms | train | test.


%%--------------------------------------------------------------------
-spec period(DataTag :: dataset()) -> proplist().
%%
%% Shortcut to get tracking run period.
%%
%% RUN-1: October 1, 2017 -- June 30, 2018              (10-fold CV)
%%--------------------------------------------------------------------
period(parms) -> [{start, {2019, 01, 01}}, {stop, {2019, 04, 01}}];
%eriod(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 07, 01}}];   % FIXME!
period(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 04, 01}}];   % FIXME!
period(test)  -> [{start, {2019, 10, 01}}, {stop, {2019, 12, 31}}].


%%--------------------------------------------------------------------
-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This `nn' function goes through the Top-N twice, using only
%       the emo/comm attributes from the higher ranking models.
%
%       This function uses weekly periods.
% @end  --
run_top_nn(Tracker, RunTag) ->

    io:format("This will destroy the current raven and player states.~n"),
    case ioo:read_down("Are you sure? ") of
        "yes" ->
            case weka:ping(raven:get_jvm_node(Tracker)) of
                timeout -> [];
                _       -> run_top_nn_aux(Tracker, RunTag)
            end;
        _ -> []
    end.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec run_top_nn_aux(Tracker :: tracker(),
                     RunTag  :: stringy()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
% @end  --
run_top_nn_aux(Tracker, RunTag) ->

    sila:reset(),
    ok = raven:emote(Tracker, period(train)),

    % We may still be waiting on messages from Weka after `emote' finishes
    Waiter = fun Recur() ->
        case raven:count_tweet_todo(Tracker) of
            0 -> ok;
            N ->
                ?info("Waiting on ~p days of tweets", [N]),
                timer:sleep(2000),
                Recur()
        end
    end,
    Waiter(),

    % Be patient.  The `player' module may have to churn a while
    ?info("Waiting for player processing to complete..."),
    Totals = #{tter := Count} = player:get_totals(Tracker, infinity),
    ?notice("Completed processing ~p tweets", [Count]),
    maps:map(fun(Comm, Cnt) ->
                 ?info("* ~s: ~B", [Comm, Cnt]) end,
             Totals),

    % Create the models
    Results = influence:run_top_nn(Tracker, RunTag, oter, fear),
    verify_run(Tracker, RunTag, train, oter, fear, Results),
    Results.


%%--------------------------------------------------------------------
-spec verify_run(Tracker :: tracker(),
                 RunTag  :: stringy(),
                 DataTag :: dataset(),
                 Comm    :: comm_code(),
                 Emo     :: emotion(),
                 Results :: proplist()) -> ack|nak|undefined.
%%
% @doc  Generates a hash representing the results of a given run.
%       If we know what the results should be (we have a hash from
%       a previous run), then we compare the current run's results
%       and warn the user they don't match up.
% @end  --
verify_run(Tracker, RunTag, DataTag, Comm, Emo, Results) ->

    Cfg = [Tracker, DataTag, Comm, Emo],
    Key = crypto:hash(sha256, term_to_binary(Cfg)),
    Val = crypto:hash(sha256, term_to_binary(Results)),

    ?info("RUN: ~s % ~p: ~p => ~p", [RunTag, Cfg, Key, Val]),
    case get_run_hash(Key) of
        none -> ?notice("No previous results to compare"),                          undefined;
        Val  -> ?notice("Results are consistent!"),                                 ack;
        Prev -> ?warning("Results do not match previous runs: prev[~s]", [Prev]),   nak
    end.



%%--------------------------------------------------------------------
-spec get_run_hash(Key :: binary()) -> none
                                     | binary().
%%
% @doc  Returns the results hash from a previous run, or `none' if
%       there is no recorded run for the specified key.
%
%       TODO: Later, we could have a better implementation with DETS.
%             However, we are expecting a good bit of by-hand tinkering
%             during this initial implementation.
% @end  --
get_run_hash(Key) ->
    RunResults = #{% [gw,train,oter,fear]}).
                   <<255,239,51,195,210,127,103,151,28,223,33,106,145,208,92,
                     54,55,176,38,83,194,9,28,181,32,124,239,51,172,52,174,213>> =>
                   <<87,81,48,74,205,149,219,162,115,48,33,140,69,53,160,50,
                     153,189,181,58,63,125,49,50,106,163,246,21,227,160,53,54>>},
    maps:get(Key, RunResults, none).
