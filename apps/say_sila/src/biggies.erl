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
         run_top_nn/2]).

-include("sila.hrl").
-include("emo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(MIN_H0_TWEETS,  2).

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
%eriod(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 07, 01}}]; % FIXME!
period(train) -> [{start, {2017, 10, 01}}, {stop, {2018, 04, 01}}]; % FIXME!
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
    % Combine the big player accounts from all categories except `tter'
    AllBigs = lists:foldl(fun ({_, {_,_,Bigs}}, Acc) -> Bigs ++ Acc end,
                          [],
                          Biggies),

    % We want to choose from users with some level of activity that are not big players
    % Fold the usernames into a map indexed by non-negative integers for random retreival
    FindMediums = fun(Usr, Info, Acc = {Meds, Cnt}) ->
        IsMedium = case Info#profile.comms of
            #{tter := Comm} ->
                (Comm#comm.cnt > ?MIN_H0_TWEETS) andalso (not lists:member(Usr, AllBigs));
            _ ->
                false
        end,

        % Toss the biggies and the extremely inactive; otherwise, include this guy.
        case IsMedium of
            false -> Acc;
            true  -> {maps:put(Cnt, Usr, Meds), Cnt+1}
        end
    end,
    MedPlayers = maps:fold(FindMediums, {#{},0}, Players),
    NumMediums = maps:size(MedPlayers),
    IndexLimit = NumMediums - 1,

    ?info("Choosing H0 players from ~B with at least ~B tweet(s)", [NumMediums,
                                                                    ?MIN_H0_TWEETS]),
    % Function to get a random medium player
    ChooseMedium = fun() ->
        Ndx = round(IndexLimit * rand:uniform()),
        maps:get(Ndx, MedPlayers)
    end,
    %rand:seed_s(exsss),       % Ref: http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf

    FindMedComm = fun ({Comm, {_,_,Bigs}}) ->
        Meds = [ChooseMedium() || _ <- Bigs],
        ?info("* ~s: ~B players", [Comm, length(Meds)]),
        {Comm, {0,0,Meds}}
    end,
    [FindMedComm(B) || B <- Biggies].



%%--------------------------------------------------------------------
-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy()) -> verification().
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
                     RunTag  :: stringy()) -> verification().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
% @end  --
run_top_nn_aux(Tracker, RunTag) ->

    % Function to pull and organize players and their tweets for a given dataset
    GetPlayers = fun(D) ->
        sila:reset(),
        ok = raven:emote(Tracker, period(D)),
        wait_on_players(Tracker),
        player:get_players(Tracker)
    end,

    % The training dataset gives the player community that deines the Top-N groups
    Players = GetPlayers(train),
    {top_n,
     Min, Max} = influence:init_range(top_n),
    TopBiggies = maps:from_list([{N, player:get_top_n(Tracker, N)} || N <- lists:seq(Min, Max)]),

    % We need player communities for the other datasets as well
    DataSets = maps:from_list([{train, Players} | [{D, GetPlayers(D)} || D <- [parms, test]]]),
    Options  = [{datasets, DataSets},
                {toppers,  TopBiggies}],

    Totals = #{tter := Count} = wait_on_players(Tracker),
    ?notice("Completed processing ~p tweets", [Count]),
    maps:map(fun(Comm, Cnt) ->
                 ?info("* ~s: ~B", [Comm, Cnt]) end,
             Totals),

    % Create the models
    Results = influence:run_top_nn(Tracker, RunTag, oter, fear, Options),
    lists:foreach(fun({N, {PCC, Attrs}}) ->
                     ?info("N @ ~2B: pcc[~7.4f] attrs~p", [N, PCC, Attrs]) end,
                  Results),
    verify_run(Tracker, RunTag, train, oter, fear, Results).



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
-spec verify_run(Tracker :: tracker(),
                 RunTag  :: stringy(),
                 DataTag :: dataset(),
                 Comm    :: comm_code(),
                 Emo     :: emotion(),
                 Results :: proplist()) -> verification().
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
        Prev -> ?warning("Results do not match previous runs: prev[~p]", [Prev]),   nak
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
    RunResults = #{% [gw,train,oter,fear]:
                   <<255,239,51,195,210,127,103,151,28,223,33,106,145,208,92,
                     54,55,176,38,83,194,9,28,181,32,124,239,51,172,52,174,213>> =>
                   <<184,7,125,26,87,144,74,217,103,168,170,58,87,116,177,140,
                     77,39,111,214,171,191,42,26,86,237,77,114,207,194,141,124>>    % 2017-10-01 to 2018-04-01
                  %<<185,37,107,157,153,126,159,159,168,222,149,163,97,88,51,
                  %  10,47,30,176,97,176,30,229,95,125,10,80,165,208,157,79,50>>    % 2017-10-01 to 2018-07-01
                  },
    maps:get(Key, RunResults, none).

