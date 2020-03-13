%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Generalized foreign node utilities for Sibyl.
%%      The interface to the node is an Erlang port.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(fnode).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").
-behaviour(gen_server).

-export([start_link/1,  start_link/2,   start_link/3,
         stop/1,
         close/1,
         call/2,        call/3,
         command/1,     command/2,
         format/2,
         open/1,
         recv/1,        recv/2,
         send_recv/2,   send_recv/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("ioo.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(RECV_TIMEOUT,   5000).


%%--------------------------------------------------------------------
-record(state, {fnode       :: stringy(),
                os_pid      :: rec_integer(),
                port        :: rec_port()}).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(FNode :: stringy()) -> gen:start_ret().

-spec start_link(FNode :: stringy(),
                 Cmd   :: stringy()) -> gen:start_ret().

-spec start_link(FNode :: stringy(),
                 Cmd   :: stringy(),
                 Args  :: [stringy()]) -> gen:start_ret().
%%
% @doc  Starts a server to handle an external process representing
%       a foreign (non-Erlang/epmd) node.
% @end  --
start_link(FNode) ->
    start_link(FNode, FNode).


start_link(FNode, Cmd) ->
    start_link(FNode, Cmd, []).


start_link(FNode, Cmd, Args) ->
    gen_server:start_link(?MODULE, [FNode, Cmd, Args], []).



%%--------------------------------------------------------------------
-spec stop(Pid :: pid()) -> ok.
%%
% @doc  Shutdown function for a DL-Learner handler.
% @end  --
stop(undefined)  ->
    ok;

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
    call(Pid, ?str_fmt(Fmt, Args)).



%%--------------------------------------------------------------------
-spec close(Port :: port()
                  | undefined) -> ok.
%%
% @doc  Closes a port to a foreign node.
% @end  --
close(undefined) -> ok;

close(Port) ->
    port_close(Port).



%%--------------------------------------------------------------------
-spec command(Cmd :: unicode:char_data()) -> string().

-spec command(Port :: rec_port(),
              Cmd  :: unicode:char_data()) -> string()
                                            | undefined.
%%
% @doc  Prepares a command for a foreign node (ensures a newline).
%
%       In the two argument version, the command is actually sent
%       to the `Port'.
% @end  --
command(Cmd) ->
    ?str_fmt("~s~n", [string:chomp(Cmd)]).


command(undefined, _) ->
    undefined;


command(Port, Cmd) ->
    Command = command(Cmd),
    port_command(Port, Command),
    Command.



%%--------------------------------------------------------------------
-spec format(Port :: port(),
             Data :: {eol|noeol, string()}) -> string().
%%
% @doc  Creates a string representing data coming into a port.
% @end  --
format(Port, {EOL, Data}) ->

    Line = case EOL of
        eol   -> <<"recv">>;
        noeol -> <<"r...">>
    end,
    ?str_fmt("~p(~s): ~s", [Port, Line, Data]).



%%--------------------------------------------------------------------
-spec open(ExtCmd :: string()) -> port().
%%
% @doc  Opens an Erlang port handling the specified external process.
% @end  --
open(ExtCmd) ->
    ?debug("OPEN: ~s", [ExtCmd]),
    open_port({spawn, ExtCmd}, [{line, 256}]).



%%--------------------------------------------------------------------
-spec recv(Port    :: rec_port()) -> port_data().
%%
% @doc  Blocks until we receive a message from `Port' for up to ten
%       seconds, and returns the received data to the caller.
% @end  --
recv(Port) ->
    recv(Port, ?RECV_TIMEOUT).


%%--------------------------------------------------------------------
-spec recv(Port    :: rec_port(),
           Timeout :: timeout()) -> port_data().
%%
% @doc  Blocks until we receive a message from `Port' for up to `Timeout'
%       milliseconds, and returns the received data to the caller.
% @end  --
recv(undefined, _) ->
    undefined;


recv(Port, Timeout) ->
    receive
        {Port, {data, Data}} ->
            ?debug(format(Port, Data)),
            Data
    after Timeout -> timeout
    end.



%%--------------------------------------------------------------------
-spec send_recv(Port :: rec_port(),
                Cmd  :: unicode:char_data()) -> {eol|noeol, string()}.
%%
% @doc  Sends a generic command to the foreign node session, blocks
%       during processing for up to ten seconds, and finally returns
%       the response to the caller.
% @end  --
send_recv(Port, Cmd) ->
    send_recv(Port, Cmd, ?RECV_TIMEOUT).



%%--------------------------------------------------------------------
-spec send_recv(Port    :: rec_port(),
                Cmd     :: unicode:char_data(),
                Timeout :: timeout()) -> port_data().
%%
% @doc  Sends a generic command to the foreign node session, blocks
%       during processing for up to `Timeout' milliseconds, and finally
%       returns the response to the caller.
% @end  --
send_recv(undefined, _, _) ->
    undefined;


send_recv(Port, Cmd, Timeout) ->

    ?debug("~p(send): ~s", [Port, Cmd]),
    command(Port, Cmd),
    recv(Port, Timeout).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initializes a process responsible for external modelling.
% @end  --
init([FNode, Cmd, Args]) ->
    ?notice("Launching ~s/~s", [FNode, Cmd]),
    process_flag(trap_exit, true),

    Port = run(FNode, Cmd, Args),
    case erlang:port_info(Port) of
        undefined ->
            ?error("Could not run DL-Learner"),
            fnode:close(Port),
            {stop, bad_run};

        Info ->
            ProcID = proplists:get_value(os_pid, Info),
            ?info("Connected ~s~p <=> ~p<os:~p>", [FNode, self(), Port, ProcID]),
            {ok, #state{fnode  = FNode,
                        port   = Port,
                        os_pid = ProcID}}
    end.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> term().
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, #state{port   = Port,
                      os_pid = ProcID}) ->

    ?notice("DL-Learner shutdown: why[~p]", [Why]),
    case ProcID of
        undefined -> ok;
        _         -> os:cmd(?str_fmt("kill ~B", [ProcID]))
    end,
    fnode:close(Port).



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
%%--------------------------------------------------------------------
-spec run(FNode :: stringy(),
          Cmd   :: stringy(),
          Args  :: [stringy()]) -> port().
%%
% @doc  Opens an Erlang port via a run-script from our priv/bin directory.
% @end  --
run(FNode, Cmd, Args) ->
    PrivDir = code:priv_dir(say_sila),
    WorkDir = ?str_fmt("~s/fnode/~s", [PrivDir, FNode]),
    ExtCmd  = ?str_fmt("~s/bin/run-~s ~s ~s", [PrivDir, Cmd, WorkDir,
                                               types:join(" ", Args)]),
    open(ExtCmd).

