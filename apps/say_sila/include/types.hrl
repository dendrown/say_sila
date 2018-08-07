%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Common dialyzer type definitions
%%
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_types_included).
-define(_types_included, ack).

-type json_binary() :: binary().

-type property()    :: atom() | {term(), term()}.
-type proplist()    :: [property()].

-type stringable()  :: atom() | binary() | string().
-type stringy()     :: atom() | binary() | string() | io_lib:chars().

-type rec_map()     :: undefined | map().
-type rec_pid()     :: undefined | pid().
-type rec_string()  :: undefined | string().
-type rec_stringy() :: undefined | stringy().


-endif.
