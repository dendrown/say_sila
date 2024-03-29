%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Time-period shortcuts for looking at data in "Say Sila".
%%
%% @copyright 2017-2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(period).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([one/0, two/0, year/0, full/0, q1/0, q2/0, q4/0, q4q1/0, today/0]).

-include("sila.hrl").
-include_lib("llog/include/llog.hrl").


%%--------------------------------------------------------------------
%% Period shortcuts
%%--------------------------------------------------------------------
one()  -> [{start, {2017, 12, 31}}, {stop, {2018, 1, 1}}].
two()  -> [{start, {2017, 12, 30}}, {stop, {2018, 1, 1}}].

year() -> [{start, {2017, 08,  1}}, {stop, {2018, 8, 1}}].
full() -> [{start, {2017, 10,  1}}, {stop, {2018, 7, 1}}].
q4()   -> [{start, {2017, 10,  1}}, {stop, {2018, 1, 1}}].   % Gonna need the year
q1()   -> [{start, {2018, 01,  1}}, {stop, {2018, 4, 1}}].
q2()   -> [{start, {2018, 04,  1}}, {stop, {2018, 7, 1}}].
q4q1() -> [{start, {2017, 10,  1}}, {stop, {2018, 4, 1}}].

today() ->
    Today = dts:dayize(calendar:local_time()),
    [{start, Today}, {stop, dts:add(Today, 1, day)}].

