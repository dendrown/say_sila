%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc FSM to handle runs with DL-Learner.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(dllearner).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").
-behaviour(gen_statem).

-export([start_link/0,
         stop/1]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([ontology/3,
         dllearner/3,
         report/3]).

-include("ioo.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


-define(RECV_TIMEOUT,   10000).                         % A bit bigger than fnode:?RECV_TIMEOUT


%%--------------------------------------------------------------------
-record(data, {fnode_pid :: rec_pid(),
               jvm_node  :: node() }).
-type data()  :: #data{}.                       % FSM internal state data


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link() -> gen_statem:start_ret().
%%
% @doc  Starts a server to handle external modelling.
% @end  --
start_link() ->
    gen_statem:start_link(?MODULE, [], []).



%%--------------------------------------------------------------------
-spec stop(Pid :: pid()) -> ok.
%%
% @doc  Shutdown function for a DL-Learner handler.
% @end  --
stop(Pid)  ->
    gen_statem:call(Pid, stop).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initializes a process responsible for external modelling.
% @end  --
init([]) ->
    ?notice("Launching DL-Learner FSM"),
    process_flag(trap_exit, true),

    % We'll be needing the JVM, so warn the sysop if we can't contact it
    gen_statem:cast(self(), is_jvm_ready),

    {ok, ontology, #data{%fnode_pid = fnode:start_link(say, dllearner), % FIXME! not yet!
                         jvm_node  = raven:get_jvm_node()}}.



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
terminate(Why, State, #data{fnode_pid = FNodePid}) ->

    ?notice("DL-Learner shutdown: st[~p] why[~p]", [State, Why]),
    fnode:stop(FNodePid).



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
% @doc  Handle state-independent events
% @end  --
handle_event(Type, Evt, _) ->
    ?warning("Received an unexpected '~p' event: type[~p]", [Evt, Type]),
    keep_state_and_data.



%%--------------------------------------------------------------------
%% ontology:
%%
% @doc  Synchronous messages for engineering knowledge.
% @end  --
ontology(enter, _, _) ->
    ?info("Constructing ontology"),
    keep_state_and_data;


ontology(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%--------------------------------------------------------------------
%% dllearner:
%%
% @doc  Synchronous messages for engineering knowledge.
% @end  --
dllearner(enter, _, _) ->
    ?info("Interfacing with DL-Learner"),
    keep_state_and_data;


dllearner(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).




%%--------------------------------------------------------------------
%% report:
%%
% @doc  Synchronous messages for engineering knowledge.
% @end  --
report(enter, _, _) ->
    ?info("Reporting results"),
    keep_state_and_data;


report(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%====================================================================
%% Internal functions
%%====================================================================
