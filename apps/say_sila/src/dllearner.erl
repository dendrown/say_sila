%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc A server to handle port-based runs with DL-Learner.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(dllearner).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").
-behaviour(gen_server).

-export([start_link/0,
         stop/1,
         call/2,    call/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("ioo.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


-define(RECV_TIMEOUT,   10000).                         % A bit bigger than fnode:?RECV_TIMEOUT
-define(dll(Fmt, Args),  io_lib:format(Fmt, Args)).     % DL-Learner command builder



%%--------------------------------------------------------------------
-record(state, {os_pid      :: rec_integer(),
                port        :: rec_port()}).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link() -> gen:start_ret().
%%
% @doc  Starts a server to handle external modelling.
% @end  --
start_link() ->
    gen_server:start_link(?MODULE, [], []).



%%--------------------------------------------------------------------
-spec stop(Pid :: pid()) -> ok.
%%
% @doc  Shutdown function for a DL-Learner handler.
% @end  --
stop(Pid)  ->
    gen_server:call(Pid, stop).



%%--------------------------------------------------------------------
-spec call(Pid :: pid(),
           Msg :: string()) -> port_data().
%%
% @doc  Sends a message to the specified DL-Learner handler.
% @end  --
call(Pid, Msg) ->
    Rsp = gen_server:call(Pid, {call, Msg}, ?RECV_TIMEOUT),
    ?debug("~s >> ~p", [Msg, Rsp]),
    Rsp.



%%--------------------------------------------------------------------
-spec call(Pid :: pid(),
           Fmt  :: string(),
           Args :: list()) -> port_data().
%%
% @doc  Creates a message from the specified format string and arguments
%       and sends it to the specified DL-Learner handler.
% @end  --
call(Pid, Fmt, Args) ->
    call(Pid, ?dll(Fmt, Args)).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initializes a process responsible for external modelling.
% @end  --
init([]) ->
    ?notice("Launching DL-Learner"),
    process_flag(trap_exit, true),

    Port = fnode:run(say, dllearner),
    case erlang:port_info(Port) of
        undefined ->
            ?error("Could not run DL-Learner"),
            fnode:close(Port),
            {stop, bad_run};

        Info ->
            ProcID = proplists:get_value(os_pid, Info),
            ?info("Connected ~p <=> ~p<os:~p>", [self(), Port, ProcID]),
            {ok, #state{port   = Port,
                        os_pid = ProcID}}
    end.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, #state{port   = Port,
                      os_pid = ProcID}) ->

    ?notice("DL-Learner shutdown: why[~p]", [Why]),
    case ProcID of
        undefined -> ok;
        _         -> os:cmd(?dll("kill ~B", [ProcID]))
    end,
    fnode:close(Port),
    Why.



%%--------------------------------------------------------------------
%% code_change:
%%
% @doc  Hot code update processing.
% @end  --
code_change(OldVsn, OldState, _Extra) ->
    ?notice("Hot code update: old[~p]", [OldVsn]),
    {ok, OldState}.



%%--------------------------------------------------------------------
%% handle_call:
%%
% @doc  Synchronous messages for engineering knowledge.
% @end  --
handle_call({call, Msg}, _From, State = #state{port = Port}) ->
    Reply = fnode:send_recv(Port, Msg),
    {reply, Reply, State};


handle_call(get_os_pid, _From, State = #state{os_pid = ProcID}) ->
    {reply, ProcID, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_cast
%%
% @doc  Asynchronous messages for the auto-trader.
% @end  --
handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info({Port, {data, Data}}, State = #state{port = Port}) ->
    ?debug(fnode:format(Port, Data)),
    {noreply, State};


handle_info({'EXIT', Port, Why}, State = #state{port = Port}) ->

    ?warning("DL-Learner at ~p terminated: ~p", [Port, Why]),
    {stop, Why, State#state{port = undefined}};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.



%%====================================================================
%% Internal functions
%%====================================================================
