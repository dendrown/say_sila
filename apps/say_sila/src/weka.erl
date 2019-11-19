%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Sila/Weka functionality
%%
%%      Weka runs on the JVM and Sila uses a Clojure/OTP node to
%%      communicate with it.  This module contains the utilities to
%%      prepare for handoffs to Weka.
%%
%% @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(weka).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([bye/1,
         exec/2,
         execute/2, execute/3,
         ping/1,
         tweet/3,
         send/2,
         send/3]).

-include("sila.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


-define(WEKA_TIMEOUT,   4900).      % Facilitate default gen_server timeouts
-define(WEKA_MAILBOX,   say).       % Clojure mailbox for the "say" fnode

-type message() :: {pid(), reference(), atom()}
                 | {pid(), reference(), atom(), string()}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec bye(Node :: node()) -> ok | timeout.
%
% @doc  Sends a command to a foreign node to cease execution
% @end  --
bye(Node) ->
    exec(Node, bye).



%%--------------------------------------------------------------------
-spec exec(Node :: node(),
           Cmd  :: atom()) -> ok | timeout.
%%
% @doc  Sends a command to the specified Weka JVM node and hangs out
%       waiting for that node's response.  The difference between this
%       function and `execute/2' (which we wrap) is that here we log
%       what happened and then simply return `ok' or `timeout'.
% @end  --
exec(Node, Cmd) ->
    case execute(Node, Cmd) of
        timeout  ->
            ?warning("Node ~s timed out", [Node]),
            timeout;
        Rsp ->
            ?info("~s: ~p", [Cmd, Rsp]),
            ok
    end.



%%--------------------------------------------------------------------
-spec execute(NodeRef :: node_reference()
                       | node(),
              Cmd     :: atom()) -> term() | timeout.


-spec execute(NodeRef :: node_reference()
                       | node(),
              Cmd     :: atom(),
              Arg     :: stringy()) -> term() | timeout.
%%
% @doc  Sends a command to the specified Weka JVM node, waits for a
%       response, and returns it.
% @end  --
execute(NodeRef, Cmd) ->
    execute(NodeRef, Cmd, no_arg).



execute(NodeRef = {_, Ref}, Cmd, Arg) ->

    % Wait for a response from the weka node
    send(NodeRef, Cmd, Arg),
    receive
        Response = {_,Ref,_,_} -> Response;
        Response = {_,Ref,_}   -> Response
    after ?WEKA_TIMEOUT ->
        timeout
    end;


execute(Node, Cmd, Arg) ->
    execute({Node, make_ref()}, Cmd, Arg).


%%--------------------------------------------------------------------
-spec ping(Node :: node()) ->  ok | timeout.
%
% @doc  Sends a command to a foreign node to cease execution
% @end  --
ping(Node) ->
    exec(Node, ping).



%%--------------------------------------------------------------------
-spec tweet(Node   :: node(),
            Name   :: string(),
            Tweets :: map() | [map()]) -> reference()
                                        | {error, term()}.
%
% @doc  Sends a command to a foreign node to cease execution
% @end  --
tweet(Node, Name, Tweets) ->
    case arff:from_tweets(Name, Tweets) of
        {ok, ARFF} ->
            {_, WorkRef, _, _} = send(Node, emote, ARFF),
            WorkRef;

        {{error, Why} = Return, ARFF} ->
            ?warning("Twort: why[~p] arff[~s]", [Why, ARFF]),
            Return
    end.



%%--------------------------------------------------------------------
-spec send(Node :: node_reference()
                 | node(),
           Cmd  :: atom()) -> message().

-spec send(Node :: node_reference()
                 | node(),
           Cmd  :: atom(),
           Arg  :: stringy()) -> message() | nonode.

%%
% @doc  Sends a command to the specified Weka JVM node.  When using
%       a node with no specified reference, these functions will
%       create a new reference to use in the message to that node.
%
%       TODO: This functionality and indeed our Clojure JVM sessions
%             in general are now more than Weka portals.  We need to
%             move this functionality into fnode.
% @end  --
send(Node, Cmd) ->
    send(Node, Cmd, no_arg).


send({Node, Ref}, Cmd, Arg) ->

    Msg = case Arg of
        no_arg  -> {self(), Ref, Cmd};
        _       -> {self(), Ref, Cmd, Arg}
    end,

    %?debug("SEND: ~p", [Msg]),
    {?WEKA_MAILBOX, Node} ! Msg;


send(Node, Cmd, Arg) ->
    send({Node, make_ref()}, Cmd, Arg).



%%====================================================================
%% Internal functions
%%====================================================================
