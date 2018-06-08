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

-export([one/0, two/0, q1/0, q4/0, q4q1/0, today/0]).

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
one()  -> [{start, {2017, 12, 31}}, {stop, {2018, 1, 1}}].
two()  -> [{start, {2017, 12, 30}}, {stop, {2018, 1, 1}}].

q4()   -> [{start, {2017, 10,  1}}, {stop, {2018, 1, 1}}].   % Gonna need the year
q1()   -> [{start, {2018, 01,  1}}, {stop, {2018, 4, 1}}].
q4q1() -> [{start, {2017, 10,  1}}, {stop, {2018, 4, 1}}].

today() ->
    Today = dts:dayize(calendar:local_time()),
    [{start, Today}, {stop, dts:add(Today, 1, day)}].

