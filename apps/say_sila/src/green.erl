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

-export([start_link/1, start_link/2,
         stop/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

% Quickies for development
-export([opts/0]).
opts() -> [no_retweet, {start, {2020, 1, 1}}, {stop, {2020, 4, 1}}].


-include("sila.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


-record(state, {tracker       :: tracker(),
                options = []  :: proplist(),
                workers = #{} :: #{pid() => proplist()} }).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: tracker()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start_link(Tracker) ->
    start_link(Tracker, opts()).



%%--------------------------------------------------------------------
-spec start_link(Tracker :: tracker(),
                 Options :: proplist()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start_link(Tracker, Options) ->
    Args = [Tracker, Options],
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, Args, []).



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
init([Tracker, Options]) ->

    ?notice("Initializing analysis of enviromentalism"),
    process_flag(trap_exit, true),

    % Get environmental tweets per the specified options
    gen_server:cast(self(), {get_tweets, Options}),

    {ok, #state{tracker = Tracker}}.



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
handle_cast({get_tweets, Options}, State = #state{tracker = Tracker}) ->
    {PeriodStart,
     PeriodStop,
     RunOpts} = daily:extract_period(Tracker, Options),

     DoWork = fun Recur(CurrDay, Acc) ->
        case daily:step(CurrDay, PeriodStop) of
            stop -> Acc;
            {NextDay,
             DayOpts} ->
                Worker = spawn_link(fun() -> get_tweets(Tracker, [DayOpts|RunOpts]) end),
                Recur(NextDay, Acc#{Worker => DayOpts})
        end
    end,

    {noreply, State#state{options = Options,
                          workers = DoWork(PeriodStart, #{})}};


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
%%--------------------------------------------------------------------
-spec get_tweets(Tracker :: tracker(),
                 Options :: proplist()) -> ok.
%%
% @doc  Retrieves and processes the tweets for a day.
% @end  --
get_tweets(Tracker, Options) ->
    ?debug("Getting ~s tweets: ~p", [Tracker, Options]).

