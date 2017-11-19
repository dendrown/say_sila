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

-define(fmt(Fmt, Args), io_lib:format(Fmt, Args)).
-define(OPTS, [{start, {2017, 10, 1}},
               {stop,  {2017, 11, 1}}]).

md(Track, Bigs) ->
    P100 = fun(Full) ->
        Tweets    = lists:filter(fun(#tweet{type=Type}) -> Type =/= retweet end, Full),
        Retweets  = lists:filter(fun(#tweet{type=Type}) -> Type =:= retweet end, Full),
        RePlayers = lists:foldl(fun(#tweet{rt_screen_name = RTSN}, Set) -> gb_sets:add_element(RTSN, Set) end,
                                    gb_sets:new(),
                                    Retweets),
        FLen = length(Full),
        {FLen,
         gb_sets:size(RePlayers),
         100 * length(Tweets)   / FLen,
         100 * length(Retweets) / FLen}
        end,
    GetMD  = fun(N) ->
        Player  = lists:nth(N, Bigs),
        ScrName = Player#player.screen_name,
        Profile = io_lib:format("[~s](https://twitter.com/~s)", [ScrName, ScrName]),
        Tweets  = twitter:get_tweets(Track, Player, ?OPTS),
        {TweetCnt, RePlayerCnt, TT100, RT100} = P100(Tweets),
        io:format("| ~-56s | ~4B | ~3B | ~3B |    ~s |~n",
                  [Profile,
                   TweetCnt,
                   round(TT100),
                   round(RT100),
                   if RePlayerCnt > 0 -> ?fmt("~4B", [RePlayerCnt]); true -> "    " end])
        end,
    io:format("| Screen Name                                              | Total| TT% | RT% |RT Accts |~n"),
    io:format("| -------------------------------------------------------- | ----:| ---:| ---:|--------:|~n"),
    lists:foreach(GetMD, lists:seq(1, length(Bigs))).
