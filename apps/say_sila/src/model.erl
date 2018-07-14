%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Modelling FSM to keep Weka in line.
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(model).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-behaviour(gen_statem).
-export([start_link/2,
         stop/1,
         go/1]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([idle/3,
         run/3,
         eval/3]).

-include("sila.hrl").
-include_lib("llog/include/llog.hrl").



%%--------------------------------------------------------------------
-record(data, {name         :: string(),
               dep_attr     :: atom(),
               ind_attrs    :: [atom()] }).
%type data() :: #data{}.                        % FSM internal state data



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Name   :: string(),
                 Target :: atom()) -> gen_statem:start_ret().
%%
% @doc  Startup function for back-off ticker services
% @end  --
start_link(Name, Target) ->
    gen_statem:start_link(?MODULE, [Name, Target], []).



%%--------------------------------------------------------------------
-spec stop(Model :: gen_statem:server_ref()) -> ok.
%%
% @doc  Shutdown the state machine.
% @end  --
stop(Model) ->
    gen_statem:call(Model, cancel),
    gen_statem:stop(Model).



%%--------------------------------------------------------------------
-spec go(Model :: gen_statem:server_ref()) -> ok.
%%
% @doc 
% @end  --
go(Model) ->
    gen_statem:cast(Model, go).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the back-off timer.
% @end  --
init([Name, Target]) ->
    %
    process_flag(trap_exit, true),
    Independents = [todo],
    {ok, idle, #data{name      = Name,
                     dep_attr  = Target,
                     ind_attrs = Independents}}.



%%--------------------------------------------------------------------
%% callback_mode:
%%
% @doc  Reports FSM callback mode.
% @end  --
callback_mode() ->
    [state_functions, state_enter].



%%--------------------------------------------------------------------
%% terminate:
%%
% @doc  FSM shutdown callback.
% @end  --
terminate(Why, _State, _Data) ->
    ?info("Model shutdown: why[~p]", [Why]),
    normal.



%%--------------------------------------------------------------------
%% code_change:
%%
% @doc  Hot code update processing: a placeholder.
% @end  --
code_change(OldVsn, _State, Data, _Extra) ->
    ?notice("Hot code update: old[~p]", [OldVsn]),
    {ok, Data}.



%%--------------------------------------------------------------------
%% handle_event:
%%
% @doc  Synchronous messages for Coinigy services
% @end  --
handle_event(cast, go, #data{name = Name}) ->
    ?warning("Model ~s got the go-ahead but is already under way", [Name]),
    keep_state_and_data;


handle_event(_, Evt, #data{name = Name}) ->
    ?warning("Model ~s received an unexpected event: ~p", [Name, Evt]),
    keep_state_and_data.



%%--------------------------------------------------------------------
%% idle:
%%
% @doc  FSM state waiting for something to do
% @end  --
idle(enter, _OldState, #data{name = Name}) ->
    %
    ?info("Model ~s is ready and waiting", [Name]),
    keep_state_and_data;


idle(cast, go, Data) ->
    {next_state,
     run,
     Data};

idle(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%--------------------------------------------------------------------
%% run:
%%
% @doc  FSM state running a Weka model
% @end  --
run(enter, _OldState, #data{name = Name}) ->
    %
    ?info("Model ~s running on Weka", [Name]),
    keep_state_and_data;


run(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%--------------------------------------------------------------------
%% eval:
%%
% @doc  FSM state running a Weka model
% @end  --
eval(enter, _OldState, #data{name = Name}) ->
    %
    ?info("Model ~s under evaluation", [Name]),
    keep_state_and_data;


eval(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%====================================================================
%% Internal functions
%%====================================================================
