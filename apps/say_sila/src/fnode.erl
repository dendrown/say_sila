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

-export([close/1,
         command/1,     command/2,
         format/2,
         open/1,
         recv/1,        recv/2,
         run/1,         run/2, run/3,
         send_recv/2,   send_recv/3]).

-include("ioo.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(RECV_TIMEOUT,   5000).


%%====================================================================
%% API
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
-spec run(FNode :: stringy()) -> port().

-spec run(FNode :: stringy(),
          Cmd   :: stringy()) -> port().

-spec run(FNode :: stringy(),
          Cmd   :: stringy(),
          Args  :: [stringy()]) -> port().
%%
% @doc  Opens an Erlang port via a run-script from our priv/bin directory.
% @end  --
run(FNode) ->
    run(FNode, FNode).


run(FNode, Cmd) ->
    run(FNode, Cmd, []).


run(FNode, Cmd, Args) ->
    PrivDir = code:priv_dir(say_sila),
    WorkDir = ?str_fmt("~s/fnode/~s", [PrivDir, FNode]),
    ExtCmd  = ?str_fmt("~s/bin/run-~s ~s ~s", [PrivDir, Cmd, WorkDir,
                                               types:join(" ", Args)]),
    open(ExtCmd).



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
%% Internal functions
%%====================================================================
