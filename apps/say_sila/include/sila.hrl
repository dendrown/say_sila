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
-define(WORK_DIR, "/srv/say_sila").

-type property()    :: atom() | {term(), term()}.
-type proplist()    :: [property()].

-define(DEFAULT_BIG_P100, 0.15).
-define(DEFAULT_PERIOD,   "day").

-define(null_val_not(X, Null, NotNull), case (X) of null -> Null;       _ -> NotNull end).
-define(null_to_undef(X),               case (X) of null -> undefined;  _ -> X       end).


-endif.
