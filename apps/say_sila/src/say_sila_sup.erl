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
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-module(say_sila_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).


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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).




%%====================================================================
%% Supervisor callbacks
%%--------------------------------------------------------------------
-spec init([atom()]) -> any().
%
% @doc  Returns the top level supervision tree
% @end  --
init([]) ->
    {ok, {{one_for_all, 0, 1},
          [{twitter, {twitter, start_link, []}, permanent, 2000, worker, [twitter]} ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
