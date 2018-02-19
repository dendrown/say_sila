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
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_dts_included).
-define(_dts_included, ack).

-type date()     :: {integer(), integer(), integer()}.
-type time()     :: {integer(), integer(), integer()}.
-type datetime() :: date() %|time()
                  | {date(), time()}.

-endif.
