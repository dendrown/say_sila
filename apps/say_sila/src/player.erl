%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc The "Say Sila" Twitter player (account) handler
%%
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(player).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/1, stop/1,
         reset/1,
         tweet/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(MODULES,    #{cc => player_cc,
                      gw => player_gw}).
-define(reg(Key), maps:get(Key, ?MODULES, ?MODULE)).

-define(MIN_COUNT,  3).                             % Minimum user activity for processing


-record(state, {tracker           :: atom(),
                player_map  = #{} :: map(),
                count_tree        :: gb_trees:tree(integer(), list()),
                tweet_total = 0   :: integer() }).   % count
-type state() :: #state{}.



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: atom()) -> {ok, pid()}
                                     |  ignore
                                     |  {error, term()}.
%%
% @doc  Startup function for Twitter account services
% @end  --
start_link(Tracker) ->
    gen_server:start_link({?REG_DIST, ?reg(Tracker)}, ?MODULE, [Tracker], []).



%%--------------------------------------------------------------------
-spec stop(Tracker :: atom()) -> ok.
%%
% @doc  Shutdown function for account services
% @end  --
stop(Tracker) ->
    gen_server:call(?reg(Tracker), stop).



%%--------------------------------------------------------------------
-spec reset(Tracker :: atom()) -> ok.
%%
% @doc  Reinitializes the state of the specified `player' server.
%%--------------------------------------------------------------------
reset(Tracker) ->
    gen_server:call(?reg(Tracker), reset).



%%--------------------------------------------------------------------
-spec tweet(Tracker :: atom(),
            Tweet   :: tweet()) -> ok.
%%
% @doc  Process a tweet, adjusting player roles as appropriate.
% @end  --
tweet(Tracker, Tweet) ->
    gen_server:cast(?reg(Tracker), {tweet, Tweet}).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Handles placing the first twig in Raven's data nest.
% @end  --
init([Tracker]) ->
    ?notice("Initializing player services: ~s", [Tracker]),
    process_flag(trap_exit, true),
    {ok, reset_state(Tracker)}.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, #state{tracker = Tracker}) ->
    ?notice("Ending player services: trk[~s] why[~p]", [Tracker, Why]),
    normal.


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
handle_call(reset, _From, #state{tracker = Tracker}) ->
    {reply, ok, reset_state(Tracker)};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_cast:
%%
% @doc  Process async messages
% @end  --
handle_cast({tweet, Tweet = #tweet{screen_name = Acct}},
                                 % rt_screen_name
                                 % type
            State = #state{player_map  = Players,
                           count_tree  = CntTree,
                           tweet_total = Total}) ->
    %?debug("Tweet: acct[~s] txt[~s]", [Acct, Tweet#tweet.text]),

    {NewPlayerMap,
     NewCountTree} = case maps:get(Acct, Players, none) of

        none ->
            % First tweet from this player, add him to the (existing) singleton node
            {maps:put(Acct, 1, Players),
             CntTree};

        AcctCnt when AcctCnt < (?MIN_COUNT-1) ->
            % Player continues tweeting, but hasn't hit our processing threshold
            {maps:put(Acct, AcctCnt + 1, Players),
             CntTree};

        AcctCnt when AcctCnt =:= (?MIN_COUNT-1) ->
            % Passing activity threshold: add user to min-count node
            % NOTE: handling this special case is faster
            %       because we know the node exists
            MinAccts = gb_trees:get(?MIN_COUNT, CntTree),
            {maps:put(Acct, ?MIN_COUNT, Players),
             gb_trees:update(?MIN_COUNT, [Acct | MinAccts], CntTree)};

        AcctCnt ->
            % Remove the account from the old count-node
            OldCntNode = gb_trees:get(AcctCnt, CntTree),
            MidCntTree = gb_trees:update(AcctCnt, lists:delete(Acct, OldCntNode), CntTree),

            % Add the account to the new count-node
            NewAcctCnt = AcctCnt + 1,
            NewPlayers = maps:put(Acct, NewAcctCnt, Players),
            NewCntTree = case gb_trees:lookup(NewAcctCnt, MidCntTree) of

                {value, CntAccts} ->
                    % Node already exists for this count
                     gb_trees:update(NewAcctCnt, [Acct | CntAccts], MidCntTree);

                none ->
                    % New node for this count
                     gb_trees:insert(NewAcctCnt, [Acct], MidCntTree)
            end,
            {NewPlayers, NewCntTree}
    end,
    {noreply, State#state{player_map  = NewPlayerMap,
                          count_tree  = NewCountTree,
                          tweet_total = Total + 1}};


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
-spec reset_state(Tracker :: atom()) -> state().
%%
% @doc  Process out-of-band messages
% @end  --
reset_state(Tracker) ->
    %
    % Start the tweet tree with a node for minimum-activity players
    CntTree = gb_trees:insert(?MIN_COUNT, [], gb_trees:empty()),
    #state{tracker = Tracker,
           count_tree = CntTree}.
