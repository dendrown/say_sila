%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc R<==>Erlang bridge for Sila
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(r).
-behaviour(gen_server).


-export([start_link/0,
         stop/0,
         eval/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").

-record(state, {eri_status :: term() }).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                    |  ignore
                    |  {error, term()}.
%%
% @doc  Startup function for Twitter services
% @end  --
start_link() ->
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, [go], []).



%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for Twitter services
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).




%%--------------------------------------------------------------------
-spec eval(Cmd :: string()) -> term().
%%
% @doc  Pass-thru evaluator for rErlang.
% @end  --
eval(Cmd) when is_list(Cmd) ->
    eri:eval(Cmd);

eval(_) ->
    % TODO: Sending commands as binaries will freeze the eri process.
    %       Make the interface more client-friendly.
    ?warning("R commands must be in string (list) form!").




%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(list()) -> any().
%%
% @doc  Initiates connection with R environment
% @end  --
init([go]) ->
    ?notice("Initializing R/Elang bridge"),
    process_flag(trap_exit, true),

    Return = case eri:start() of
        true ->
            gen_server:cast(self(), connect),
            {ok, #state{}};
        Fail ->
            {stop, Fail}
    end,
    Return.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _State) ->
    ?notice("Shutting down the R/Erlang bridge: why[~p]", [Why]),
    eri:stop(),
    normal.


%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(),
                  State  :: state(),
                  Extra  :: term()) -> {atom(), term()}.
%%
% @doc  Hot code update processing: a placeholder.
% @end  --
code_change(OldVsn, State, _Extra) ->
    ?notice("Hot code update: old[~p]", [OldVsn]),
    {ok, State}.


%%--------------------------------------------------------------------
%% handle_call:
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% handle_cast
%%
% @doc  Process async messages
% @end  --
handle_cast(connect, State) ->
    Eri = eri:connect(),
    Action = case Eri of
        {ok, 0} ->
            ?info("Connected to R environment"),
            noreply;
        Fail ->
            ?info("Failed to connect to R environment: why[~p]", [Fail]),
            stop
    end,
    {Action, State#state{eri_status = Eri}};


handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================
