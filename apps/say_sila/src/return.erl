%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Utilities for processing return values.
%%
%% @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(return).

-export([error_or_first/1]).


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec error_or_first(Returns :: list()) -> none
                                         | term()
                                         | {error, term()}.
%%
% @doc  Return the first `error' or the first element (generally `ok')
% @end  --
error_or_first([]) ->
    none;

error_or_first(Returns) ->
    case proplists:lookup(error, Returns) of
        none  -> hd(Returns);
        Error -> Error
    end.



%%====================================================================
%% Internal functions
%%====================================================================
