%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc A try-it-out playpen for looking at data in "Say Sila".
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(adhoc).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([md/2]).

-include("twitter.hrl").

-define(OPTS, [{start, {2017, 10, 1}}]).

md(Track, Bigs) ->
    P100 = fun(Full) ->
        FLen = length(Full),
        {FLen,
         100 * length(lists:filter(fun(#tweet{type=Type}) -> Type =/= retweet end, Full)) / FLen,
         100 * length(lists:filter(fun(#tweet{type=Type}) -> Type =:= retweet end, Full)) / FLen}
        end,
    GetMD  = fun(N) ->
        Player = lists:nth(N, Bigs),
        Tweets = twitter:get_tweets(Track, Player, ?OPTS),
        {Count, TT100, RT100} = P100(Tweets),
        io:format("| ~-16s | ~4B | ~3B | ~3B |~n", [Player#player.screen_name,
                                                    Count,
                                                    round(TT100),
                                                    round(RT100)])
        end,
    io:format("| Screen Name      | Total|  TT |  RT |~n"),
    io:format("| CreativeCivil    | ----:| ---:| ---:|~n"),
    lists:foreach(GetMD, lists:seq(1, length(Bigs))).
