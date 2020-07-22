%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Date/Timestamp utilities
%%
%% @copyright 2018-2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_dts_included).
-define(_dts_included, ack).

-define(DTS_EPOCH,      {{1970,01,01},{00,00,00}}).
-define(SECS_IN_MIN,    60).
-define(SECS_IN_HOUR,  (60 * ?SECS_IN_MIN)).
-define(SECS_IN_DAY,   (24 * ?SECS_IN_HOUR)).

-define(MILLIS_IN_MIN,  60000).
-define(MILLIS_IN_HOUR, (60 * ?MILLIS_IN_MIN)).
-define(MILLIS_IN_DAY,  (24 * ?MILLIS_IN_HOUR)).

-type time_unit() :: millisecond | second | minute | hour | day.
-type dts_1970()  :: non_neg_integer().
-type date()      :: {integer(), integer(), integer()}.
-type time()      :: {integer(), integer(), integer()}.
-type datetime()  :: date() %|time()
                   | {date(), time()}.

-endif.
