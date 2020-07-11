%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila functionality for analysis of environmentalism.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(green).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/0,
         stop/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

% Quickies for development
-export([opts1/0]).
opts1() -> [no_retweet, {start, {2020, 1, 1}}, {stop, {2020, 4, 1}}].


-include("sila.hrl").
-include_lib("llog/include/llog.hrl").


-record(state, {todo :: atom() }).
-type state() :: #state{}.

%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                    |  ignore
                    |  {error, term()}.
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start_link() ->
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, none, []).



%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for modelling enviromentalism.
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the Twitter access server.
% @end  --
init(none) ->

    ?notice("Initializing analysis of enviromentalism"),
    process_flag(trap_exit, true),

    {ok, #state{}}.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _) ->
    Why.



%%--------------------------------------------------------------------
%% code_change:
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
