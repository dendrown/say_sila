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
-include("wui.hrl").


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
%% init:
%
% @doc  Returns the top level supervision tree
% @end  --
init([]) ->
    WuiConf = wui:get_conf(),

    % TODO: Consider `cc' and `gw' supervisors responsible for `raven'/`player'
    %       groups, and then just refer to processes in the group by pid.
    {ok, {{one_for_one, 5, 60},
          [{twitter,    {twitter, start_link, []},        permanent, 2000, worker, [twitter]},
          %{r,          {r,       start_link, []},        permanent, 2000, worker, [r]},
           {raven_cc,   {raven,   start_link, [cc]},      permanent, 2000, worker, [raven]},
           {raven_gw,   {raven,   start_link, [gw]},      permanent, 2000, worker, [raven]},
           {player_cc,  {player,  start_link, [cc]},      permanent, 2000, worker, [player]},
           {player_gw,  {player,  start_link, [gw]},      permanent, 2000, worker, [player]},
           {wui,        {wui,     start_link, [WuiConf]}, transient, 1000, worker, [wui]}
           | WuiConf#yaws_conf.childSpecs]}}.


%%====================================================================
%% Internal functions
%%====================================================================
