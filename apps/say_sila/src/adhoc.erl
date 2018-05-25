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

-include("emo.hrl").
-include("player.hrl").
-include("twitter.hrl").

-define(fmt(Fmt, Args), io_lib:format(Fmt, Args)).
-define(out(Fmt, Args), io:format(Fmt, Args)).
-define(terpri(),       io:put_chars("\n")).
-define(twitter(Acct),  io_lib:format("[~s](https://twitter.com/~s)", [Acct, Acct])).

-define(SN,   <<"SCREEN NAME">>).
-define(OPTS, [{start, {2017, 10, 1}},
               {stop,  {2017, 11, 1}}]).


%%--------------------------------------------------------------------
md(Track, P100) ->
    % Default `md' report
    md(rpt_comms, Track, P100).


%%--------------------------------------------------------------------
md(rpt_comms, Track, P100) ->
    %
    % NOTE: raven:emote/2 must have been called at this point...
    Players = player:get_players(Track),
    BigTalk = player:get_big_p100(Track, P100),
    LineOut = fun(Acct) ->
                 #profile{cnts = Cnts,
                          emos = Emos} = maps:get(Acct, Players),
                 % Sanity check
                 Note = case (Cnts#counts.tt =:= Emos#emotions.count) of
                    true  -> <<>>;
                    false -> ?fmt("COUNT MISMATCH (tt[~B] =/= emo[~B])",
                                  [Cnts#counts.tt,
                                   Emos#emotions.count])
                 end,
                 ?out("| ~-56s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s |~s~n", [?twitter(Acct)]
                                                                               ++ cnts_str(Cnts)
                                                                               ++ emos_str(Emos)
                                                                               ++ [Note])
                 end,
    Report = fun(Ndx) ->
                 Type = lists:nth(Ndx, ?COMM_TYPES),
                 Code = lists:nth(Ndx, ?COMM_CODES),
                 {Pct,
                  Cnt,
                  Accts} = proplists:get_value(Code, BigTalk),
                 ?terpri(),
                 ?out("### `~s` communications (~s): pct[~.2f%] cnt[~B]~n", [twitter:to_hashtag(Track),
                                                                             Type,
                                                                             Pct * 100.0,
                                                                             Cnt]),
                 ?terpri(),
                 ?out("| ~-56.. s |   TT  |  OT% |  RT   |  TM   |  ANGR |  FEAR |  SAD  |  JOY  |~n", [?SN]),
                 ?out("| ~-56..-s | -----:| ----:|------:| -----:| -----:| -----:| -----:| -----:|~n", [<<>>]),
                 lists:foreach(LineOut, Accts),
                 ?terpri()
                 end,
    lists:foreach(Report, lists:seq(1, length(?COMM_CODES), 1));


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
        Tweets  = twitter:get_tweets(Track, Player, ?OPTS),
        {TweetCnt, RePlayerCnt, TT100, RT100} = P100(Tweets),
        io:format("| ~-56s | ~4B | ~3B | ~3B |    ~s |~n",
                  [?twitter(ScrName),
                   TweetCnt,
                   round(TT100),
                   round(RT100),
                   if RePlayerCnt > 0 -> ?fmt("~4B", [RePlayerCnt]); true -> "    " end])
        end,
    io:format("| Screen Name                                              | Total| TT% | RT% |RT Accts |~n"),
    io:format("| -------------------------------------------------------- | ----:| ---:| ---:|--------:|~n"),
    lists:foreach(GetMD, lists:seq(1, length(Bigs))).



 %%--------------------------------------------------------------------
cnts_str(Counts = #counts{tt = TT}) ->
    %
    [counts | CntList] = tuple_to_list(Counts),
    lists:map(fun({Key, Val}) ->
                  case {Key, 0 =:= Val} of
                      {ot, true} when TT > 0 -> <<"  0%">>;
                      {ot, true}             -> <<"    ">>;
                      {ot, false}            -> ?fmt("~3B%", [round(100.0 * Val / TT)]);
                      { _, false}            -> ?fmt("~5B",  [Val]);
                      { _, true}             -> <<"     ">>
                  end end,
              lists:zip(record_info(fields, counts), CntList)).



%%--------------------------------------------------------------------
emos_str(Emos) ->
    lists:map(fun(Key) -> emo_str(Key, Emos) end, ?EMOTIONS).


emo_str(Key, Emos) ->
    Level = ?emo_level(Key, Emos),
    case emo:is_stoic(Level) of
        true  -> <<"     ">>;
        false -> ?fmt("~5.2f", [Level])
    end.



%%--------------------------------------------------------------------
one() -> [{start, {2017, 12, 31}}, {stop, {2018, 1, 1}}].
two() -> [{start, {2017, 12, 30}}, {stop, {2018, 1, 1}}].
q4()  -> [{start, {2017, 10,  1}}, {stop, {2018, 1, 1}}].

today() ->
    Today = dts:dayize(calendar:local_time()),
    [{start, Today}, {stop, dts:add(Today, 1, day)}].

