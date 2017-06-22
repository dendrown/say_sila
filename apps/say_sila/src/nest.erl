%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc The twitter data nest
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(nest).
-behaviour(gen_server).


-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").

-record(state, {todo   :: any() }).
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




%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(list()) -> any().
%%
% @doc  Handles placing the first twig in the Twitter nest.
% @end  --
init([go]) ->
    ?notice("Welcome to Say Sila's Twitter Nest"),
    process_flag(trap_exit, true),

    gen_server:cast(self(), authenticate),

    {ok, #state{}}.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _State) ->
    ?notice("Shutting down the nest: why[~p]", [Why]),
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
-spec handle_call(Msg   :: term(),
                  From  :: {pid(), term()},
                  State :: state()) -> any().
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
-spec handle_cast(Msg   :: term(),
                  State :: state()) -> any().
%%
% @doc  Process async messages
% @end  --
handle_cast(authenticate, State) ->
    URL = twitter:get_pin(),
    io:format("Please retrieve our PIN from ~s~n", [URL]),
    {ok, PIN} = io:fread("PIN> ", "~s"),
    twitter:authenticate(PIN),
    {noreply, State};


handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
-spec handle_info(Msg   :: term(),
                  State :: term()) -> any().
%%
% @doc  Process out-of-band messages
% @end  --
handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================
