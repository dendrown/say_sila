%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Common definitions for Sila
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_sila_included).
-define(_sila_included, ack).

-define(REG_DIST, local).
-define(WORK_DIR, "/tmp/sila").

-type property()    :: atom() | {term(), term()}.
-type proplist()    :: [property()].

-endif.
