%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Functional programming tools
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(fp).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([remove/2]).

-import(lists, [filter/2]).


%%--------------------------------------------------------------------
-spec remove(Pred :: fun((term()) -> boolean()),
             List :: list()) ->list().
%%
% @doc  Removes elements from List for which the specified predicate
%       returns true.
% @end  --
remove(Pred, List) ->
    filter(fun(X) -> not Pred(X) end,
           List).

