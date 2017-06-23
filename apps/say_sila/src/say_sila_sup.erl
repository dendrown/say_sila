%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila top level supervisor
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(say_sila_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("sila.hrl").


%%====================================================================
%% API functions
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                    | ignore
                    | {error, term()}.
%
% @doc  Starts uptop level supervisor
% @end  --
start_link() ->
    supervisor:start_link({?REG_DIST, ?MODULE}, ?MODULE, []).




%%====================================================================
%% Supervisor callbacks
%%--------------------------------------------------------------------
-spec init([atom()]) -> any().
%
% @doc  Returns the top level supervision tree
% @end  --
init([]) ->
    {ok, {{one_for_all, 5, 60},
          [{twitter, {twitter, start_link, []}, permanent, 2000, worker, [twitter]},
           {nest,    {nest,    start_link, []}, permanent, 2000, worker, [nest]} ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
