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

-export([md/2, md/3, one/0, two/0, q4/0, today/0]).

-include("player.hrl").
-include("twitter.hrl").

-define(fmt(Fmt, Args), io_lib:format(Fmt, Args)).
-define(out(Fmt, Args), io:format(Fmt, Args)).
-define(terpri(),       io:put_chars("\n")).

-define(OPTS, [{start, {2017, 10, 1}},
               {stop,  {2017, 11, 1}}]).


%%--------------------------------------------------------------------
md(Track, P100) ->
    Counts = player:get_big_p100(Track, P100),
    Report = fun(Ndx) ->
                 Type = lists:nth(Ndx, ?COMMS_TYPES),
                 Code = lists:nth(Ndx, ?COMMS_CODES),
                 {Pct,
                  Cnt,
                  Accts} = proplists:get_value(Code, Counts),

                 ?out("Comms<~s>: pct[~.2f%] cnt[~B]~n", [Type,
                                                          Pct * 100.0,
                                                          Cnt]),
                 lists:foreach(fun(Acct) -> ?out("  ~s~n", [Acct]) end, Accts),
                 ?terpri()
                 end,
    lists:foreach(Report, lists:seq(1, length(?COMMS_CODES), 1)).



%%--------------------------------------------------------------------
md(rpt_tt_rt, Track, Bigs) ->
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



%%--------------------------------------------------------------------
one() -> [{start, {2017, 12, 31}}, {stop, {2018, 1, 1}}].
two() -> [{start, {2017, 12, 30}}, {stop, {2018, 1, 1}}].
q4()  -> [{start, {2017, 10,  1}}, {stop, {2018, 1, 1}}].

today() ->
    Today = dts:dayize(calendar:local_time()),
    [{start, Today}, {stop, dts:add(Today, 1, day)}].

