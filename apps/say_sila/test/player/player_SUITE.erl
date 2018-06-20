%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Tweet Emotion Analyzer for Climate Change
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(player_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([get_players_01/1,
         get_rankings_01/1,
         tweet_01/1,
         tweet_02/1,
         tweet_03/1,
         tweet_04/1]).

-include("player.hrl").
-include("twitter.hrl").

-define(TRACKER,    gw).
-define(MANY_COMMS, (10 * ?MIN_COMMS_COUNT)).


%%%-------------------------------------------------------------------
-define(TWEET,   #tweet{id              = <<"947256553513549824">>,
                        screen_name     = <<"TestUser85">>,
                        timestamp_ms    = 1514678536190,
                        type            = tweet,
                        rt_id           = undefined,
                        rt_screen_name  = undefined,
                        emotions        = emo:stoic(),
                        text = <<"Looks like a wind chill of -35 for #NewYearsEve. Must be all that "
                                 "#climatechange and #globalwarming at work. https://t.co/XD1z9HA40e">>}).

-define(RETWEET, #tweet{id              = <<"947256127091290112">>,
                        screen_name     = <<"realghess1">>,
                        timestamp_ms    = 1514678434523,
                        type            = retweet,
                        rt_id           = <<"945426445006004224">>,
                        rt_screen_name  = <<"_CFJ_">>,
                        emotions        = emo:stoic(),
                        text = <<"RT @_CFJ_: This Day In #GlobalWarming History\n\n"
                                 "COLDEST #Christmas Day EVER in Kelowna TODAY https://t.co/7zkEe1KASS">>}).

-define(MENTION, #tweet{id              = <<"947256368951582720">>,
                        screen_name     = <<"AndyOz2">>,
                        timestamp_ms    = 1514678492187,
                        type            = tweet,
                        rt_id           = undefined,
                        rt_screen_name  = undefined,
                        emotions        = emo:stoic(),
                        text = <<"#fakenews @CNN says there is record cold &amp; snow. \n"
                                 "I guess some #GlobalWarming would be welcome.">>}).


%%--------------------------------------------------------------------
all() ->
    [get_players_01,
     get_rankings_01,
     tweet_01, tweet_02, tweet_03, tweet_04].


%%--------------------------------------------------------------------
init_per_testcase(_, Config) ->
    player:start_link(?TRACKER),
    Config.


%%--------------------------------------------------------------------
end_per_testcase(_, _Config) ->
    player:stop(?TRACKER).


%%--------------------------------------------------------------------
get_players_01(_Config) ->
    #{} =:= player:get_players(?TRACKER).


%%--------------------------------------------------------------------
get_rankings_01(_Config) ->
     Rankings = player:get_rankings(?TRACKER),
     Rankings = #{tter => {1,{?MIN_COMMS_COUNT, [], nil,nil}},
                  oter => {1,{?MIN_COMMS_COUNT, [], nil,nil}},
                  rter => {1,{?MIN_COMMS_COUNT, [], nil,nil}},
                  rted => {1,{?MIN_COMMS_COUNT, [], nil,nil}},
                  tmed => {1,{?MIN_COMMS_COUNT, [], nil,nil}}}.



%%--------------------------------------------------------------------
tweet_01(_Config) ->
    %
    ok = tweet_N_times(?MIN_COMMS_COUNT, ?TWEET),
    Account = string:lowercase(?TWEET#tweet.screen_name),

    Players = player:get_players(?TRACKER),
    Profile = maps:get(Account, Players),
    check_comm_counts(Profile, [tter, oter], ?MIN_COMMS_COUNT),
    check_comm_counts(Profile, [rter, rted, tmed], undefined),

    Rankings = player:get_rankings(?TRACKER),
    Rankings = #{tter => {1,{?MIN_COMMS_COUNT, [Account],nil, nil}},
                 oter => {1,{?MIN_COMMS_COUNT, [Account],nil, nil}},
                 rter => {1,{?MIN_COMMS_COUNT, [],       nil, nil}},
                 rted => {1,{?MIN_COMMS_COUNT, [],       nil, nil}},
                 tmed => {1,{?MIN_COMMS_COUNT, [],       nil, nil}}},

    Totals = player:get_totals(?TRACKER),
    Totals = #{tter => ?MIN_COMMS_COUNT,
               oter => ?MIN_COMMS_COUNT,
               rter => 0,
               rted => 0,
               tmed => 0}.


%%--------------------------------------------------------------------
tweet_02(_Config) ->
    %
    ok = tweet_N_times(?MIN_COMMS_COUNT, ?RETWEET),
    Account = string:lowercase(?RETWEET#tweet.screen_name),
    Author  = string:lowercase(?RETWEET#tweet.rt_screen_name),

    Players = player:get_players(?TRACKER),
    check_comm_counts(maps:get(Account, Players), [tter, rter], ?MIN_COMMS_COUNT),
    check_comm_counts(maps:get(Author,  Players), rted, ?MIN_COMMS_COUNT),

    Rankings = player:get_rankings(?TRACKER),
    Rankings = #{tter => {1,{?MIN_COMMS_COUNT,[Account], nil, nil}},
                 oter => {1,{?MIN_COMMS_COUNT,[],        nil, nil}},
                 rter => {1,{?MIN_COMMS_COUNT,[Account], nil, nil}},
                 rted => {1,{?MIN_COMMS_COUNT,[Author],  nil, nil}},
                 tmed => {1,{?MIN_COMMS_COUNT,[],        nil, nil}}},

    Totals = player:get_totals(?TRACKER),
    Totals = #{tter => ?MIN_COMMS_COUNT,
               oter => 0,
               rter => ?MIN_COMMS_COUNT,
               rted => ?MIN_COMMS_COUNT,
               tmed => 0}.



%%--------------------------------------------------------------------
tweet_03(_Config) ->
    %
    ok = tweet_N_times(?MIN_COMMS_COUNT, ?MENTION),
    Account = string:lowercase(?MENTION#tweet.screen_name),
    Mention = string:lowercase(<<"CNN">>),

    Players = player:get_players(?TRACKER),
    check_comm_counts(maps:get(Account, Players), [tter, oter], ?MIN_COMMS_COUNT),
    check_comm_counts(maps:get(Mention, Players), tmed, ?MIN_COMMS_COUNT),

    Rankings = player:get_rankings(?TRACKER),
    Rankings = #{tter => {1, {?MIN_COMMS_COUNT, [Account],  nil, nil}},
                 oter => {1, {?MIN_COMMS_COUNT, [Account],  nil, nil}},
                 rter => {1, {?MIN_COMMS_COUNT, [],         nil, nil}},
                 rted => {1, {?MIN_COMMS_COUNT, [],         nil, nil}},
                 tmed => {1, {?MIN_COMMS_COUNT, [Mention], nil, nil}}},

    Totals = player:get_totals(?TRACKER),
    Totals = #{tter => ?MIN_COMMS_COUNT,
               oter => ?MIN_COMMS_COUNT,
               rter => 0,
               rted => 0,
               tmed => ?MIN_COMMS_COUNT}.



%%--------------------------------------------------------------------
tweet_04(_Config) ->
    %
    ok = tweet_N_times(?MANY_COMMS, ?TWEET),
    Account = string:lowercase(?TWEET#tweet.screen_name),

    Players = player:get_players(?TRACKER),
    Profile = maps:get(Account, Players),
    check_comm_counts(Profile, [tter, oter], ?MANY_COMMS),
    check_comm_counts(Profile, [rter, rted, tmed], undefined),

    Rankings  = player:get_rankings(?TRACKER),
    [Account] = gb_trees:get(?MANY_COMMS, maps:get(tter, Rankings)),

    Totals = player:get_totals(?TRACKER),
    Totals = #{tter => ?MANY_COMMS,
               oter => ?MANY_COMMS,
               rter => 0,
               rted => 0,
               tmed => 0}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
tweet_N_times(N, Tweet) ->
    %
    ok = lists:foreach(fun(_) -> player:tweet(?TRACKER, Tweet) end,
                       lists:seq(1, N, 1)).


check_comm_counts(Profile, CommCode, Goodness) when is_atom(CommCode) ->
    Comm = maps:get(CommCode, Profile#profile.comms),
    Comm#comm.cnt =:= Goodness;


check_comm_counts(_, [], _) ->
    true;


check_comm_counts(Profile, [CommCode| RestCodes], undefined) ->
    %
    case maps:get(CommCode, Profile#profile.comms, undefined) of
        undefined -> check_comm_counts(Profile, RestCodes, undefined);
        _GotValue -> false
    end;


check_comm_counts(Profile, [CommCode| RestCodes], Goodness) ->
    Comm = maps:get(CommCode, Profile#profile.comms),
    case Comm#comm.cnt =:= Goodness of
        false -> false;
        true  -> check_comm_counts(Profile, RestCodes, Goodness)
    end.
