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
         tweet_01/1,
         tweet_02/1,
         tweet_03/1]).

-include("player.hrl").
-include("twitter.hrl").

-define(TRACKER, gw).
-define(TWEET,   #tweet{id              = <<"947256553513549824">>,
                        screen_name     = <<"TestUser85">>,
                        timestamp_ms    = 1514678536190,
                        type            = tweet,
                        emotions        = undefined,
                        rt_id           = undefined,
                        rt_screen_name  = undefined,
                        text = <<"Looks like a wind chill of -35 for #NewYearsEve. Must be all that "
                                 "#climatechange and #globalwarming at work. https://t.co/XD1z9HA40e">>}).

-define(RETWEET, #tweet{id              = <<"947256127091290112">>,
                        screen_name     = <<"realghess1">>,
                        timestamp_ms    = 1514678434523,
                        type            = retweet,
                        emotions        = undefined,
                        rt_id           = <<"945426445006004224">>,
                        rt_screen_name  = <<"_CFJ_">>,
                        text = <<"RT @_CFJ_: This Day In #GlobalWarming History\n\n"
                                 "COLDEST #Christmas Day EVER in Kelowna TODAY https://t.co/7zkEe1KASS">>}).

-define(MENTION, #tweet{id              = <<"947256368951582720">>,
                        screen_name     = <<"AndyOz2">>,
                        timestamp_ms    = 1514678492187,
                        type            = tweet,
                        emotions        = undefined,
                        rt_id           = undefined,
                        rt_screen_name  = undefined,
                        text = <<"#fakenews @CNN says there is record cold &amp; snow. \n"
                                 "I guess some #GlobalWarming would be welcome.">>}).


%%--------------------------------------------------------------------
all() ->
    [get_players_01, tweet_01, tweet_02, tweet_03].


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
tweet_01(_Config) ->
    ok = player:tweet(?TRACKER, ?TWEET),
    Account = ?TWEET#tweet.screen_name,
    Players = player:get_players(?TRACKER),
    Players = #{Account => #counts{tt = 1, rt = 0, tm = 0}}.


%%--------------------------------------------------------------------
tweet_02(_Config) ->
    ok = player:tweet(?TRACKER, ?RETWEET),
    Account = ?RETWEET#tweet.screen_name,
    Author  = ?RETWEET#tweet.rt_screen_name,
    Players = player:get_players(?TRACKER),
    Players = #{Account => #counts{tt = 1, rt = 0, tm = 0},
                Author  => #counts{tt = 0, rt = 1, tm = 0}}.


%%--------------------------------------------------------------------
tweet_03(_Config) ->
    ok = player:tweet(?TRACKER, ?MENTION),
    Account = ?MENTION#tweet.screen_name,
    Players = player:get_players(?TRACKER),
    Players = #{Account  => #counts{tt = 1, rt = 0, tm = 0},
               <<"CNN">> => #counts{tt = 0, rt = 0, tm = 1}}.


