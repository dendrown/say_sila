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
     Rankings = #counts{tt = {1,{3,[],nil,nil}},
                        ot = {1,{3,[],nil,nil}},
                        rt = {1,{3,[],nil,nil}},
                        tm = {1,{3,[],nil,nil}}}.


%%--------------------------------------------------------------------
tweet_01(_Config) ->
    %
    ok = tweet_N_times(?MIN_COMMS_COUNT, ?TWEET),
    Account = ?TWEET#tweet.screen_name,

    Players = player:get_players(?TRACKER),
    #profile{cnts = #counts{tt = ?MIN_COMMS_COUNT,
                            ot = ?MIN_COMMS_COUNT,
                            rt = 0,
                            tm = 0}} = maps:get(Account, Players),

    Rankings = player:get_rankings(?TRACKER),
    Rankings = #counts{tt = {1,{?MIN_COMMS_COUNT, [Account],nil, nil}},
                       ot = {1,{?MIN_COMMS_COUNT, [Account],nil, nil}},
                       rt = {1,{?MIN_COMMS_COUNT, [],       nil, nil}},
                       tm = {1,{?MIN_COMMS_COUNT, [],       nil, nil}}},

    #counts{tt = ?MIN_COMMS_COUNT,
            rt = 0,
            tm = 0} = player:get_totals(?TRACKER).



%%--------------------------------------------------------------------
tweet_02(_Config) ->
    %
    ok = tweet_N_times(?MIN_COMMS_COUNT, ?RETWEET),
    Account = ?RETWEET#tweet.screen_name,
    Author  = ?RETWEET#tweet.rt_screen_name,

    Players = player:get_players(?TRACKER),
    #profile{cnts = #counts{tt = ?MIN_COMMS_COUNT,
                            ot = 0,
                            rt = 0,
                            tm = 0}} = maps:get(Account, Players),
    #profile{cnts = #counts{tt = 0,
                            ot = 0,
                            rt = ?MIN_COMMS_COUNT,
                            tm = 0}} = maps:get(Author, Players),

    Rankings = player:get_rankings(?TRACKER),
    Rankings = #counts{tt = {1,{?MIN_COMMS_COUNT,[Account],     nil, nil}},
                       ot = {1,{?MIN_COMMS_COUNT,[],            nil, nil}},
                       rt = {1,{?MIN_COMMS_COUNT,[<<"_CFJ_">>], nil, nil}},
                       tm = {1,{?MIN_COMMS_COUNT,[],            nil, nil}}},

    #counts{tt = ?MIN_COMMS_COUNT,
            ot = 0,
            rt = ?MIN_COMMS_COUNT,
            tm = 0} = player:get_totals(?TRACKER).


%%--------------------------------------------------------------------
tweet_03(_Config) ->
    %
    ok = tweet_N_times(?MIN_COMMS_COUNT, ?MENTION),
    Account = ?MENTION#tweet.screen_name,

    Players = player:get_players(?TRACKER),
    #profile{cnts = #counts{tt = ?MIN_COMMS_COUNT,
                            ot = ?MIN_COMMS_COUNT,
                            rt = 0,
                            tm = 0}} = maps:get(Account, Players),
    #profile{cnts = #counts{tt = 0,
                            ot = 0,
                            rt = 0,
                            tm = ?MIN_COMMS_COUNT}} = maps:get(<<"CNN">>, Players),

    Rankings = player:get_rankings(?TRACKER),
    Rankings = #counts{tt = {1, {?MIN_COMMS_COUNT, [Account],   nil, nil}},
                       ot = {1, {?MIN_COMMS_COUNT, [Account],   nil, nil}},
                       rt = {1, {?MIN_COMMS_COUNT, [],          nil, nil}},
                       tm = {1, {?MIN_COMMS_COUNT, [<<"CNN">>], nil, nil}}},

    #counts{tt = ?MIN_COMMS_COUNT,
            ot = ?MIN_COMMS_COUNT,
            rt = 0,
            tm = ?MIN_COMMS_COUNT} = player:get_totals(?TRACKER).


%%--------------------------------------------------------------------
tweet_04(_Config) ->
    %
    ok = tweet_N_times(?MANY_COMMS, ?TWEET),
    Account = ?TWEET#tweet.screen_name,
    Players = player:get_players(?TRACKER),
    #profile{cnts = #counts{tt = ?MANY_COMMS,
                            ot = ?MANY_COMMS,
                            rt = 0,
                            tm = 0}} = maps:get(Account, Players),
    Rankings  = player:get_rankings(?TRACKER),
    [Account] = gb_trees:get(?MANY_COMMS, Rankings#counts.tt),

    #counts{tt = ?MANY_COMMS,
            ot = ?MANY_COMMS,
            rt = 0,
            tm = 0} = player:get_totals(?TRACKER).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
tweet_N_times(N, Tweet) ->
    %
    ok = lists:foreach(fun(_) -> player:tweet(?TRACKER, Tweet) end,
                       lists:seq(1, N, 1)).
