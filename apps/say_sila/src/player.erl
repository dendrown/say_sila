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
%% @copyright 2018-2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(player).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/1,      stop/1,
         clear_cache/0,
         get_biggies/2,     get_biggies/3,
         get_big_p100/2,
         get_big_venn/2,
         get_comm_codes/0,  get_comm_codes/1,
         get_comm_combos/0,
         get_players/1,
         get_rankings/1,
         get_top_n/2,
         get_totals/1,      get_totals/2,
         load/2,
         plot/1,            plot/2, plot/3,
         reset/1,
         save/2,
         tweet/2,
         update_comm/2,
        %----------------------
        % Pending final design:
        %----------------------
         ontologize/1,
         say_ontology/2]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(MODULES,    #{cc => player_cc,
                      gw => player_gw}).
-define(reg(Key),   maps:get(Key, ?MODULES, ?MODULE)).

% DETS tables
-define(DETS_FSTUB,             ?WORK_DIR "/dets/player_").
-define(PLAYERS_CACHE,          ?DETS_FSTUB "players").
-define(RANKINGS_CACHE,         ?DETS_FSTUB "rankings").
-define(TOTALS_CACHE,           ?DETS_FSTUB "totals").
-define(DETS_CACHES,            [?PLAYERS_CACHE, ?RANKINGS_CACHE, ?TOTALS_CACHE]).

%% Plotting definitions
-define(PLOT_MARKERS,           [0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.50]).
-define(PLOT_BP_RATES,          [0.0025, 0.0050, 0.0075, 0.0100, 0.0125, 0.0150, 0.0175, 0.0200]).
-define(PLOT_MIN_DECEL,         1.0e09).
-define(PLOT_NUM_PLAYERS,       #{cc => 300,
                                  gw => 300}).
-define(plot_num_players(Trk),  maps:get(Trk, ?PLOT_NUM_PLAYERS)).

-define(PLOT_NUM_TWEETS,        #{cc => 16000,
                                  gw =>  1500}).
-define(plot_num_tweets(Trk),   maps:get(Trk, ?PLOT_NUM_TWEETS)).


%%--------------------------------------------------------------------
-record(state, {tracker     :: tracker(),
                players     :: map(),       % map(key=acct, val=profile)
                rankings    :: map(),
                totals      :: map(),       % counts of tter|oter|rter|rted|tmed
                jvm_node    :: atom() }).
-type state() :: #state{}.
-type words() :: [binary()].



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: tracker()) -> {ok, pid()}
                                     |  ignore
                                     |  {error, term()}.
%%
% @doc  Startup function for Twitter account services.
% @end  --
start_link(Tracker) ->
    InitCache = fun(C) ->
        ioo:make_fpath(C),
        dets:open_file(C, [{repair, true}, {auto_save, 60000}])
    end,
    [InitCache(C) || C <- ?DETS_CACHES],
    gen_server:start_link({?REG_DIST, ?reg(Tracker)}, ?MODULE, [Tracker], []).



%%--------------------------------------------------------------------
-spec stop(Tracker :: tracker()) -> ok.
%%
% @doc  Shutdown function for account services.
% @end  --
stop(Tracker) ->
    gen_server:call(?reg(Tracker), stop).



%%--------------------------------------------------------------------
-spec clear_cache() -> ok
                     | {error, term()}.
%%
% @doc  Clears player-related cache.
% @end  --
clear_cache() ->
    return:error_or_first([dets:delete_all_objects(C) || C <- ?DETS_CACHES]).




%%--------------------------------------------------------------------
-spec get_big_p100(Tracker :: tracker(),
                   BigP100 :: float()) -> proplist().
%%
% @doc  Returns top P100 percent
%
% @deprecated   The flat percentage strategy does not provide
%               common cut points for different player types.
%               Use {@link get_biggies}
%               This function will be gone after we convert the
%               Venn Diagrammer and some code from adhoc.
% @end  --
get_big_p100(_, BigP100) when   BigP100 =< 0.0
                         orelse BigP100 >= 1.0 ->
    % Tell them what we want a percentage value to look like.
    ?error("Specify percentage between 0 and 1"),
    error(badarg);


get_big_p100(Tracker, BigP100) ->
    %
    Totals = get_totals(Tracker),
    Ranks  = get_rankings(Tracker),

    % Figure out the counts which represent the desired percentage
    % NOTE: We're doing two list comprehensions for the sake of the log message
    GoalCnts = [{Comm, round(BigP100 * Tot)} || {Comm,Tot} <- maps:to_list(Totals)],

    ?info("Big P100 goals: pct[~4.1f%] goals~p", [100*BigP100, GoalCnts]),
    Goals = [{Comm,
              Cnt,
              maps:get(Comm, Ranks)} || {Comm,Cnt} <- GoalCnts],

    % Pull the accounts which get us as close as we can to our goal counts
    BigActivity  = lists:map(fun get_top/1, Goals),

    % Put it all together and map across all communication codes
    Finalizer = fun({Comm, {Cnt, Accts}}) ->
                    Pct = Cnt / maps:get(Comm, Totals),
                    {Comm, {Pct, Cnt, Accts}}
                    end,
    lists:map(Finalizer, BigActivity).



%%--------------------------------------------------------------------
-spec get_big_venn(Tracker :: tracker(),
                   BigP100 :: float()) -> proplist().
%%
% @doc  Generate data for a Venn diagram for the WUI.
% @end  --
get_big_venn(Tracker, BigP100) ->
    %
    BigP100s = get_big_p100(Tracker, BigP100),

    % Create account sets for each of the communication codes
    AcctSets = lists:map(fun(Comm) ->
                             {_,_, Accts} = proplists:get_value(Comm, BigP100s),
                             {Comm, sets:from_list(Accts)}
                             end,
                         ?COMM_CODES),

    % Function to create counts for a list of one or more codes
    Intersect = fun(Comms) ->
                    Accts = [proplists:get_value(C, AcctSets) || C <- Comms],
                    ISet  = sets:intersection(Accts),
                    %?debug("INTERSECTION~p => ~p", [Comms, sets:to_list(ISet)]),
                    {Comms, sets:size(ISet)}
                    end,

    % Make it so...
    [Intersect(Comms) || Comms <- get_comm_combos()].



%%--------------------------------------------------------------------
-spec get_biggies(Tracker :: tracker(),
                  MinRate :: float()) -> proplist().
%%
% @doc  Returns big players, candidate influencers
% @end  --
get_biggies(Tracker, MinRate) ->
    get_biggies(Tracker, MinRate, ?PLOT_MIN_DECEL).



%%--------------------------------------------------------------------
-spec get_biggies(Tracker  :: tracker(),
                  MinRate  :: float(),
                  MinDecel :: float()) -> proplist().
%%
% @doc  Returns big players, candidate influencers
% @end  --
get_biggies(_, MinRate, _) when   MinRate =< 0.0
                           orelse MinRate >= 1.0 ->
    % Tell them what we want a percentage value to look like.
    ?error("Specify percentage between 0 and 1"),
    error(badarg);


get_biggies(Tracker, MinRate, MinDecel) ->
    %
    Totals = get_totals(Tracker),
    Ranks  = get_rankings(Tracker),

    % Pull the accounts which are adding tweets at or above the minimum rate
    BigChecker  = fun(Comm) ->
                      CommTotal = maps:get(Comm, Totals),
                      CommRanks = maps:get(Comm, Ranks),
                      {Cnt, Accts} = pull_biggies(MinRate, MinDecel, CommTotal, CommRanks),
                      {Comm, {Cnt/CommTotal,
                              Cnt,
                              Accts}}
                      end,
    lists:map(BigChecker, ?COMM_CODES).



%%--------------------------------------------------------------------
-spec get_comm_codes() -> [comm_code()].
%%
% @doc  Returns a list of defined communication codes.
% @end  --
get_comm_codes() ->
    ?COMM_CODES.



%%--------------------------------------------------------------------
-spec get_comm_codes(N :: integer()) -> [list()].
%%
% @doc  Returns communication codes in tuple-groups of N.
% @end  --
get_comm_codes(N) ->
    get_comm_combos(N, get_comm_codes()).



%%--------------------------------------------------------------------
-spec get_comm_combos() -> [list()].
%%
% @doc  Returns a list of combinations defined communication codes.
% @end  --
get_comm_combos() ->
    Combiner = fun Recur(0) -> [];
                   Recur(N) -> Recur(N-1) ++ get_comm_codes(N) end,
    Combiner(length(?COMM_CODES)).



%%--------------------------------------------------------------------
-spec get_players(Tracker :: tracker()) -> map().
%%
% @doc  Returns the server's internal players map.
%%--------------------------------------------------------------------
get_players(Tracker) ->
    gen_server:call(?reg(Tracker), get_players).



%%--------------------------------------------------------------------
-spec get_rankings(Tracker :: tracker()) -> map().
%%
% @doc  Returns the server's internal count rankings.
%%--------------------------------------------------------------------
get_rankings(Tracker) ->
    gen_server:call(?reg(Tracker), get_rankings).



%%--------------------------------------------------------------------
-spec get_top_n(Tracker :: tracker(),
                N       :: pos_integer()) -> proplist().
%%
% @doc  Returns a list of the top N big players, candidate influencers,
%       as a property list keyed by communication code.  Each property
%       contains a tuple with the percentage of that category's tweets 
%       represented by the top N players, the number of tweets that the
%       players are responsible for, and finally a list of the players
%       themselves.  (This is the same data/format as `get_biggies'.
% @end  --
get_top_n(Tracker, N) ->
    %
    Totals   = get_totals(Tracker),
    Rankings = get_rankings(Tracker),

    % Start by grabbing the top N accounts with their tweet counts
    TopReducer = fun Recur(TweetTotal, Ranks, {AcctCnt, TweetCnt, Accts}) ->
                     % 
                     % NOTE: When we hit N accounts, we add the tweet percentage
                     %       to the final returned tuple.
                     if AcctCnt =:= N ->
                            {TweetCnt/TweetTotal, TweetCnt, Accts};
                        AcctCnt > N ->
                            ?warning("Same activity rate for N=~B, returning ~B accounts",
                                     [N, AcctCnt]),
                            {TweetCnt/TweetTotal, TweetCnt, Accts};
                        true ->
                            {RankTweets,
                             RankAccts,
                             NewRanks} = gb_trees:take_largest(Ranks),
                             NumAccts  = length(RankAccts),
                             NumTweets = NumAccts * RankTweets,
                             Recur(TweetTotal, NewRanks, {AcctCnt  + NumAccts,
                                                          TweetCnt + NumTweets,
                                                          RankAccts ++ Accts})
                     end end,
    [{Comm, TopReducer(maps:get(Comm, Totals),
                       maps:get(Comm, Rankings), {0, 0, []})} || Comm <- ?COMM_CODES].



%%--------------------------------------------------------------------
-spec get_totals(Tracker :: tracker()) -> map().

-spec get_totals(Tracker :: tracker(),
                 Timout  :: infinity
                          | non_neg_integer()) -> map().
%%
% @doc  Returns the server totals in a `comm' map with keys:
%           - `tter'  total count of all tweets sent
%           - `oter'  number of original posts tweeted
%           - `rter'  number of postes retweeted
%           - `rted'  number of times an author is retweeted
%           - `tmed'  number of times users are mentioned
%
%       We provide a version with a timeout because some clients will
%       use this function to wait until processing completes to check
%       the results and continue with the next step for their goal.
%%--------------------------------------------------------------------
get_totals(Tracker) ->
    get_totals(Tracker, 5000).


get_totals(Tracker, Timeout) ->
    gen_server:call(?reg(Tracker), get_totals, Timeout).



%%--------------------------------------------------------------------
-spec load(Tracker :: tracker(),
           Period  :: proplist()) -> [{player|ranking|totals, non_neg_integer()}]
                                      | none.
%%
% @doc  Retrives player information from the DETS cache.
% @end  --
load(Tracker, Period) ->

    Key = {Tracker, lists:sort(Period)},
    ?info("Checking cache for ~p", [Key]),

    % Function to pull info from the DETS tables and return it as a proplist
    Dig = fun
        Recur([], Acc) ->
            Acc;
        Recur([{Cache,Item}|Rest], Acc) ->
            case dets:lookup(Cache, Key) of
                []           ->[];
                [{Key,Data}] -> Recur(Rest, [{Item,Data}|Acc])
            end
    end,

    case Dig([{?PLAYERS_CACHE,  players},
              {?RANKINGS_CACHE, rankings},
              {?TOTALS_CACHE,   totals}], []) of
        []   -> none;
        Info -> gen_server:call(?reg(Tracker), {set_info, Info})
    end.



%%--------------------------------------------------------------------
-spec ontologize(Tracker :: tracker()) -> ok.
%%
% @doc  Finalizes ontology creation by sending post counts for the
%       original tweeters.  (Maybe there'll be more to come...)
% @end  --
ontologize(Tracker) ->
    gen_server:call(?reg(Tracker), ontologize).



%%--------------------------------------------------------------------
-spec plot(Tracker  :: tracker()) -> ok.
%%
% @doc  Creates gnuplot scripts, data files and images.
% @end  --
plot(Tracker) ->
    plot(Tracker, ?PLOT_BP_RATES, ?PLOT_MIN_DECEL).



%%--------------------------------------------------------------------
-spec plot(Tracker  :: tracker(),
           MinDecel :: float()) -> ok.
%%
% @doc  Creates gnuplot scripts, data files and images.
% @end  --
plot(Tracker, MinDecel) ->
    plot(Tracker, ?PLOT_BP_RATES, MinDecel).



%%--------------------------------------------------------------------
-spec plot(Tracker  :: tracker(),
           Rates    :: float | [float()],
           MinDecel :: float()) -> ok.
%%
% @doc  Creates gnuplot scripts, data files and images.
% @end  --
plot(Tracker, Rate, MinDecel) when is_float(Rate) ->
    plot(Tracker, [Rate], MinDecel);


plot(Tracker, Rates, MinDecel) ->

    Players  = get_players(Tracker),
    Rankings = get_rankings(Tracker),
    Biggies  = [get_biggies(Tracker, Rate, MinDecel) || Rate <- Rates],
    NullComm = ?NEW_COMM,

    % Draw lines on the plot at certain percentile points
    Marker = fun(Code) ->
                 InfoByComm = [proplists:get_value(Code, InfoByRate) || InfoByRate <- Biggies],
                 [length(BPs) || {_Pct, _Cnt, BPs} <- InfoByComm]
                 end,

    Plotter  = fun(Code) ->
                   % How many accounts participated in the current communication mode?
                   Accounter = fun(_Acct, #profile{comms = Comms}, Acc) ->
                                   Comm = maps:get(Code, Comms, NullComm),
                                   case  Comm#comm.cnt > 0 of
                                       true  -> Acc + 1;
                                       false -> Acc
                                    end end,
                   AcctCnt = maps:fold(Accounter, 0, Players),
                   Ranks   = maps:get(Code, Rankings),
                   Markers = Marker(Code),
                   plot(Tracker, Code, AcctCnt, Ranks, Markers)
                   end,

    lists:foreach(Plotter, maps:keys(Rankings)).



%%--------------------------------------------------------------------
-spec reset(Tracker :: tracker()) -> ok.
%%
% @doc  Reinitializes the state of the specified `player' server.
%%--------------------------------------------------------------------
reset(Tracker) ->
    gen_server:call(?reg(Tracker), reset).



%%--------------------------------------------------------------------
-spec save(Tracker :: tracker(),
            Period  :: proplist()) -> ok
                                    | {error, term()}.
%%
% @doc  Inserts current player information into the DETS cache.
% @end  --
save(Tracker, Period) ->
    Players = get_players(Tracker),
    case maps:size(Players) of
        0 ->
            ?warning("No players to cache"),
            {error, empty};
        _ ->
            Key = {Tracker, lists:sort(Period)},
            ?info("Storing ~p to cache", [Key]),
            Ins = [dets:insert(C, {Key,Data}) || {C,Data} <- [{?PLAYERS_CACHE,  Players},
                                                              {?RANKINGS_CACHE, get_rankings(Tracker)},
                                                              {?TOTALS_CACHE,   get_totals(Tracker)}]],
        return:error_or_first(Ins)
    end.




%%--------------------------------------------------------------------
-spec tweet(Tracker :: tracker(),
            Tweet   :: tweet()) -> ok.
%%
% @doc  Process a tweet, adjusting player roles as appropriate.
% @end  --
tweet(Tracker, Tweet) ->
    gen_server:cast(?reg(Tracker), {tweet, Tweet}).



%%--------------------------------------------------------------------
-spec update_comm(Comm   :: comm(),
                  Update :: comm()
                          | emos()) -> comm().
%%
% @doc  Updates the first communication info block with a second
%       block or a raw emotion record.
% @end  --
update_comm(Comm = #comm{cnt  = Cnt,
                         emos = Emos}, Emotions = #emos{}) ->
    % FIXME: We already have a count in emos
    Comm#comm{cnt  = 1 + Cnt,
              emos = emo:average(Emos, Emotions)};


update_comm(Comm = #comm{cnt = Cnt1, emos = Emos1},
                   #comm{cnt = Cnt2, emos = Emos2}) ->
    % FIXME: We already have a count in emos
    Comm#comm{cnt  = Cnt1 + Cnt2,
              emos = emo:average(Emos1, Emos2)}.



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Handles placing the first twig in Raven's data nest.
% @end  --
init([Tracker]) ->
    ?notice("Initializing player services: trk[~s]", [Tracker]),
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
handle_call(get_players, _From, State = #state{players = Players}) ->
    {reply, Players, State};


handle_call(get_rankings, _From, State = #state{rankings = Rankings}) ->
    {reply, Rankings, State};


handle_call(get_totals, _From, State = #state{totals = Totals}) ->
    {reply, Totals, State};


handle_call(reset, _From, #state{tracker = Tracker}) ->
    % Invoke garbage collection and shrink our memory footprint
    {reply, ok, reset_state(Tracker), hibernate};


handle_call(ontologize, _From, State = #state{rankings = Rankings,
                                              jvm_node = JVM}) ->

    % Functions to run a ranking tree, sending role declarations to Clojure
    RecPosts = fun(Cnt, Acct) ->
                   Ont = #{domain   => Acct,
                           property => hasPostCount,
                           range    => Cnt},
                   say_ontology(JVM, jsx:encode([Ont])),
                   ok end,

    GBRunner = fun Recur(none) -> ok;
                   Recur({Cnt, Accts, Itr}) ->
                      [RecPosts(Cnt, Acct) || Acct <- Accts],
                      Recur(gb_trees:next(Itr))
                      end,

    % Only do (posting) tweeters:
    % Retweeted and Mentioned Authors were handled during normal processing
    TreeItr = gb_trees:iterator(maps:get(tter, Rankings)),
    GBRunner(gb_trees:next(TreeItr)),
    {reply, ok, State};


handle_call({set_info, Info}, _From, State) ->
    GetField = fun(F) ->
        proplists:get_value(F, Info, #{})
    end,
    Counts   = [{F, maps:size(M)} || {F,M} <- Info],
    NewState = State#state{players  = GetField(players),
                           rankings = GetField(rankings),
                           totals   = GetField(totals)},
    {reply, Counts, NewState};


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
handle_cast({tweet, Tweet = #tweet{screen_name  = ScreenName,
                                   timestamp_ms = TweetMillis,
                                   type         = Type}},
            State = #state{players  = Players,
                           rankings = Rankings,
                           totals   = Totals,
                           jvm_node = JVM}) ->

    Account = string:lowercase(ScreenName),
    KeyDTS  = dts:dayize(TweetMillis, millisecond),
    %?info("TWEET: dts[~B] id[~s] type[~s] acct[~s]", [KeyDTS, Tweet#tweet.id, Type, Account]),

    % Update the tweet counter(s) and rank(s) for this tweet
     CommCodes = case Type of
        tweet   -> [tter, oter];
        retweet -> [tter, rter]
    end,
    MidPlayers  = update_players(Account,  KeyDTS, CommCodes, Tweet, Players),
    MidRankings = update_rankings(Account, CommCodes, MidPlayers, Rankings),
    MidTotals   = update_totals(CommCodes, Totals),

    % And update the social network counts/rankings
    {NewPlayers,
     NewRankings,
     NewTotals} = check_network(KeyDTS, Tweet, JVM, MidPlayers, MidRankings, MidTotals),

    {noreply, State#state{players   = NewPlayers,
                          rankings  = NewRankings,
                          totals    = NewTotals}};


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
-spec get_comm_combos(N     :: integer(),
                      Comms :: [comm_code()]) -> [list()].
%%
% @doc  Returns all defined pairs of communication codes
%
%       REF: https://stackoverflow.com/questions/47114104/
%       REF: https://panduwana.wordpress.com/2010/04/21/combination-in-erlang/
% @end  --
get_comm_combos(_, []) ->
    [];


get_comm_combos(1, Comms) ->
    [[C] || C <- Comms];


get_comm_combos(2, [Comm | Rest]) ->
    [[Comm, C] || C <- Rest] ++ get_comm_combos(2, Rest);


get_comm_combos(N, [Comm | Rest]) ->
    [[Comm | Subs] || Subs <- get_comm_combos(N-1, Rest)] ++ get_comm_combos(N, Rest).



%%--------------------------------------------------------------------
-spec new_ranking() -> count_tree().
%%
% @doc  Creates a new ranking structure.
% @end  --
new_ranking() ->
    % Start the tweet tree with a node for minimum-activity players
    gb_trees:insert(?MIN_COMMS_COUNT, [], gb_trees:empty()).



%%--------------------------------------------------------------------
-spec reset_state(Tracker :: tracker()) -> state().
%%
% @doc  (Re)initialize the server state data
% @end  --
reset_state(Tracker) ->

    % Grab what we need from the configuration
    JVM = case application:get_application() of
        undefined -> jvm@nonode;
        {ok, App} -> application:get_env(App, jvm_node, jvm@nonode)
    end,

    % Start the tweet tree with a node for minimum-activity players
    NumComms = length(?COMM_CODES),
    CommInit = lists:zip(?COMM_CODES, lists:seq(1, NumComms)),
    RankInit = new_ranking(),
    ReRanker = fun({Comm,_}) -> {Comm, RankInit} end,
    ReZeroer = fun({Comm,_}) -> {Comm, 0} end,

    #state{tracker  = Tracker,
           players  = #{},
           rankings = maps:from_list([ReRanker(C) || C <- CommInit]),
           totals   = maps:from_list([ReZeroer(C) || C <- CommInit]),
           jvm_node = JVM}.



%%--------------------------------------------------------------------
-spec get_top({Comm    :: comm_code(),
               GoalCnt :: non_neg_integer(),
               Ranking :: count_tree()}) -> {comm_code(),
                                             {non_neg_integer(), list()}}.
%%
% @doc  Attempts to get the accounts with a total as close to the goal
%       as possible.
% @end  --
get_top({Comm, GoalCnt, Ranking}) ->
    {Comm, get_top(GoalCnt, Ranking, 0, [])}.



%%--------------------------------------------------------------------
-spec get_top(GoalCnt :: non_neg_integer(),
              Ranking :: count_tree(),
              Cnt     :: non_neg_integer(),
              Accts   :: list()) -> {non_neg_integer(), list()}.
%%
% @doc  Attempts to get the accounts with a total as close to the goal
%       as possible.  This is the helper function for get_top/2.
% @end  --
get_top(GoalCnt, Ranking, Cnt, Accts) ->
    %
    case gb_trees:is_empty(Ranking) of

        true ->
            % Nothing more we can do, return what we've got
            {Cnt, Accts};

        false ->
            % Pull the (next) biggest group
            {TopCnt,
             TopAccts,
             NewRanking} = gb_trees:take_largest(Ranking),

            NewCnt   = Cnt + (TopCnt * length(TopAccts)),
            NewAccts = Accts ++ TopAccts,

            %?debug("Getting top accounts: goal[~B] cnt[~B => ~B] tree[~B => ~B]",
            %       [GoalCnt, Cnt, NewCnt, gb_trees:size(Ranking), gb_trees:size(NewRanking)]),

             % Have we found enough?
             case NewCnt >= GoalCnt of
                true  -> {NewCnt, NewAccts};
                false -> get_top(GoalCnt, NewRanking, NewCnt, NewAccts)
            end
        end.



%%--------------------------------------------------------------------
-spec get_total(Account  :: binary(),
                CommCode :: comm_code(),
                Players  :: map()) -> non_neg_integer().
%%
% @doc  Returns the total activity for the specified account for the
%       indicated communication type.
% @end  --
get_total(Account, CommCode, Players) ->
    %
    Prof = maps:get(Account, Players),
    Comm = maps:get(CommCode, Prof#profile.comms, ?NEW_COMM),
    Comm#comm.cnt.



%%--------------------------------------------------------------------
-spec pull_biggies(MinRate  :: float(),
                   MinDecel :: float(),
                   Total    :: non_neg_integer(),
                   Ranking  :: count_tree()) -> {non_neg_integer(), list()}.
%%
% @doc  Attempts to get the accounts with a total as close to the goal
%       as possible.
% @end  --
pull_biggies(MinRate, MinDecel, Total, Ranking) ->
    pull_biggies(MinRate, MinDecel, Total, Total, Ranking, 0, []).



%%--------------------------------------------------------------------
-spec pull_biggies(MinRate  :: float(),
                   MinDecel :: float(),
                   Total    :: non_neg_integer(),
                   TallyIn  :: non_neg_integer(),
                   RanksIn  :: count_tree(),
                   CountIn  :: non_neg_integer(),
                   AcctsIn  :: list()) -> {non_neg_integer(), list()}.
%%
% @doc  Attempts to get the accounts with a total as close to the goal
%       as possible.  This is the helper function for get_top/2.
% @end  --
pull_biggies(MinRate, MinDecel, Total, TallyIn, RanksIn, CountIn, AcctsIn) ->
    %
    case gb_trees:is_empty(RanksIn) of

        true ->
            % Nothing more we can do, return what we've got
            {CountIn, AcctsIn};

        false ->
            % Pull the (next) biggest group
             case gb_trees:take_largest(RanksIn) of

                {_, [], Ranks} ->
                    % Make sure this isn't an empty node in the tree
                    pull_biggies(MinRate, MinDecel, Total, TallyIn, Ranks, CountIn, AcctsIn);

                {Tally, Accts, Ranks} ->
                    % Is this group still tweeting above the required contribution rate
                    % and deceleration?  Note for the calculations, our delta-player is 1.
                    Rate  = Tally / Total,                      %  percentage/∂player
                    Decel = (TallyIn - Tally) / (CountIn + 1),  % ∂percentage/∂player
                    case (Rate  >= MinRate)  orelse
                         (Decel >= MinDecel) of
                        true ->
                            % We got a group of biggies, add 'em and try to pull more...
                            Count    = CountIn + (Tally * length(Accts)),
                            NewAccts = AcctsIn ++ Accts,
                            %?debug("Pulling biggies: rate[~6.4f] decel[~6.4f] cnt[~B => ~B] tree[~B => ~B]",
                            %       [Rate, Decel, CountIn, Count, gb_trees:size(RanksIn), gb_trees:size(Ranks)]),

                            pull_biggies(MinRate, MinDecel, Total, Tally, Ranks, Count, NewAccts);
                        false ->
                            % This group didn't make the cut.  We're done!
                            {CountIn, AcctsIn}
                    end
            end
    end.



%%--------------------------------------------------------------------
-spec update_players(Account   :: binary(),
                     KeyDTS    :: non_neg_integer(),
                     CommCodes :: comm_code()
                                | comm_codes(),
                     Tweet     :: tweet(),
                     Players   :: map()) -> map().
%%
% @doc  Retrieves a player's `profile' from the `Players' map, updates it
%       with respect to the specified `Tweet', and returns the new map.
% @end  --
update_players(Account, KeyDTS, CommCode, Tweet, Players) when is_atom(CommCode) ->
    %
    update_players(Account, KeyDTS, [CommCode], Tweet, Players);


update_players(Account, KeyDTS, CommCodes, Tweet, Players) ->

    % The (current) profile implementation is a map of communications
    Emotions = Tweet#tweet.emotions,

    Profiler = fun(Code, Profile = #profile{comms = AllComms,
                                            lots  = AllLots}) ->
                   % NOTE: When we start using #comm.msgs, consider cutting
                   %       the text from stoic tweets to limit memory usage.
                   NewAllComm = update_comm(maps:get(Code, AllComms, ?NEW_COMM), Emotions),

                   % Remember `lots' looks like map(K=day, V=map(K=code, V=comm))
                   LotComms    = maps:get(KeyDTS, AllLots, #{}),
                   NewLotComm  = update_comm(maps:get(Code, LotComms, ?NEW_COMM), Emotions),
                   NewLotComms = maps:put(Code, NewLotComm, LotComms),

                   Profile#profile{comms = maps:put(Code, NewAllComm, AllComms),
                                   lots  = maps:put(KeyDTS, NewLotComms, AllLots)}
                   end,

    NewProfile = lists:foldl(Profiler, maps:get(Account, Players, #profile{}), CommCodes),

    % And Our players map contains the comms maps
    maps:put(Account, NewProfile, Players).



%%--------------------------------------------------------------------
-spec check_network(KeyDTS   :: non_neg_integer(),
                    Tweet    :: tweet(),
                    JVM      :: atom(),
                    Players  :: map(),
                    Rankings :: map(),
                    Totals   :: map()) -> {map(), map(), map()}.
%%
% @doc  Updates a `Players' map according to the retweet/mention counts
%       in the specified `Tweet'.
% @end  --
check_network(KeyDTS,
              Tweet = #tweet{text = Text},
              JVM,
              Players,
              Rankings = #{rted := RanksRTed, tmed := RanksTMed},
              Totals   = #{rted := TotalRTed, tmed := TotalTMed}) ->

    Words = string:split(Text, " ", all),

    {MidWords,
     MidPlayers,
     NewRanksRTed,
     NewTotalRTed} = check_retweet(KeyDTS, Tweet, Words, JVM, Players, RanksRTed, TotalRTed),

    {NewPlayers,
     NewRanksTMed,
     NewTotalTMed} = check_mentions(KeyDTS, Tweet, MidWords, JVM, MidPlayers, RanksTMed, TotalTMed),

    {NewPlayers,
     maps:put(rted, NewRanksRTed, maps:put(tmed, NewRanksTMed, Rankings)),
     maps:put(rted, NewTotalRTed, maps:put(tmed, NewTotalTMed, Totals))}.



%%--------------------------------------------------------------------
-spec check_retweet(KeyDTS  :: non_neg_integer(),
                    Tweet   :: tweet(),
                    Words   :: words(),
                    JVM     :: atom(),
                    Players :: map(),
                    Ranking :: count_tree(),
                    Total   :: non_neg_integer()) -> {words(), map(), count_tree(), non_neg_integer()}.

%%
% @doc  Updates the `Players' map with respect to a retweeted author
%       if the specified `Tweet' is a `retweet'.  If it is, the first
%       two words are stripped from the word list in the return tuple.
% @end  --
check_retweet(KeyDTS,
              Tweet = #tweet{type           = retweet,
                             id             = ID,
                             rt_screen_name = ScreenNameRT,
                             screen_name    = Acct},
              [<<"RT">>, <<$@, RetweetedTag/binary>> | RestWords],
              _JVM,
              Players,
              Ranking,
              Total) ->
    %
    Author  = string:lowercase(ScreenNameRT),
    AuthRef = string:lowercase(RetweetedTag),

    % Pull off the colon from the author reference to verify the retweeted author
    {NewPlayers,
     NewRanking,
     NewTotal} = case binary:split(AuthRef, <<":">>, [trim]) of

        [Author] ->
            % Updates the retweeted author's (NOT the tweeter's) counts/ranking
            MidPlayers = update_players(Author, KeyDTS, rted, Tweet, Players),

            % Inform the say-sila ontology about the retweet
            % TODO: say_ontology(JVM, twitter:ontologize(Tweet, json)),

            RetweetedCnt = get_total(Author, rted, MidPlayers),
            {MidPlayers,
             update_ranking(Author, RetweetedCnt, Ranking),
             1 + Total};

        [Who] ->
            ?warning("Cannot verify retweet: id[~s] acct[~s] auth[~p =/= ~p]",
                     [ID, Acct, Author, Who]),
            {Players, Ranking, Total}
    end,
    {RestWords, NewPlayers, NewRanking, NewTotal};


check_retweet(_, #tweet{type           = retweet,
                        id             = ID,
                        rt_screen_name = Author,
                        screen_name    = Acct}, Words, _, Players, Ranking, Total) ->
    %
    ?warning("Non-standard retweet text: id[~p] acct[~s] auth[~s]", [ID, Acct, Author]),
    {Words, Players, Ranking, Total};


check_retweet(_, _, Words, _, Players, Ranking, Total) ->
    % Ignore non-retweets
    {Words, Players, Ranking, Total}.



%%--------------------------------------------------------------------
-spec check_mentions(KeyDTS  :: non_neg_integer(),
                     Tweet   :: tweet(),
                     Words   :: words(),
                     JVM     :: atom(),
                     Players :: map(),
                     Ranking :: count_tree(),
                     Total   :: non_neg_integer()) -> {map(), count_tree(), non_neg_integer()}.
%%
% @doc  Updates the `Players' map and the mentions (`tm') `Rankings'
%       with respect to the accounts mentioned in the word list from
%       the tweet text.
% @end  --
check_mentions(_, _, [], _, Players, Ranking, Total) ->
    {Players, Ranking, Total};


check_mentions(KeyDTS, Tweet, [<<$@>> | RestWords], JVM, Players, Ranking, Total) ->
    %
    % Skip lone @-sign
    check_mentions(KeyDTS, Tweet, RestWords, JVM, Players, Ranking, Total);


check_mentions(KeyDTS,
               Tweet, % = #tweet{id = ID, screen_name = Acct},
               [<<$@, MentionTag/binary>> | RestWords],
               JVM,
               Players,
               Ranking,
               Total) ->
    % Updates the mentioned account's (NOT the tweeter's) counts/ranking
    Mention    = string:lowercase(MentionTag),
    NewPlayers = update_players(Mention, KeyDTS, tmed, Tweet, Players),

    % Inform the say-sila ontology about the mention
    % TODO: This code is waiting on a (more) final design
    %TwID = list_to_binary(?str_fmt("t~s", [ID])),               % Prefix for Clj-var
    %Roles = [#{domain => Acct, property => makesMentionIn, range => TwID},
    %         #{domain => TwID, property => hasMentionOf,   range => Mention}],
    %say_ontology(JVM, jsx:encode(Roles)),

    % Look for more mentions in the rest of the tweet text
    MentionCnt = get_total(Mention, tmed, NewPlayers),
    check_mentions(KeyDTS,
                   Tweet,
                   RestWords,
                   JVM,
                   NewPlayers,
                   update_ranking(Mention, MentionCnt, Ranking),
                   1 + Total);


check_mentions(KeyDTS, Tweet, [ _ | RestWords], JVM, Players, Ranking, Total) ->
    % Normal word
    check_mentions(KeyDTS, Tweet, RestWords, JVM, Players, Ranking, Total).



%%--------------------------------------------------------------------
-spec update_rankings(Account   :: binary(),
                      CommCodes :: comm_codes(),
                      Players   :: map(),
                      Rankings  :: map()) -> map().
%%
% @doc  Updates the account's ranking in the count tree.
%
%       NOTE: We assume the `Account' exists in the `Players' maps,
%             so update `Players' before calling this function.
% @end  --
update_rankings(Account, CommCodes, Players, Rankings) ->

    % We update the Players map first, so this guy should already exist
    Profile = maps:get(Account, Players),
    Ranker  = fun(Code, AccRanks) ->
                  Comm  = maps:get(Code, Profile#profile.comms),
                  Ranks = maps:get(Code, Rankings),
                  maps:put(Code,
                           update_ranking(Account, Comm#comm.cnt, Ranks),
                           AccRanks)
                  end,

    lists:foldl(Ranker, Rankings, CommCodes).



%%--------------------------------------------------------------------
-spec update_totals(CommCodes :: comm_code()
                               | comm_codes(),
                    Totals    :: map()) -> map().
%%
% @doc  Increments the `Totals' counts for the specified `CommCodes'.
% @end  --
update_totals(CommCode, Totals) when is_atom(CommCode) ->
    %
    NewCnt = 1 + maps:get(CommCode, Totals, 0),
    maps:put(CommCode, NewCnt, Totals);


update_totals(CommCodes, Totals) when is_list(CommCodes) ->
    %
    lists:foldl(fun update_totals/2, Totals, CommCodes).



%%--------------------------------------------------------------------
-spec update_ranking(Acct    :: binary(),
                     CommCnt :: non_neg_integer(),
                     Ranking :: count_tree()) -> count_tree().
%%
% @doc  Updates the account's ranking in the count tree.
%
%       NOTE: We assume the account exists in the `Players' map,
%             so update `Players' before calling this function.
% @end  --
update_ranking(Acct, CommCnt, Ranking) ->
    %
    %?debug("Rank update: acct[~s] cnt[~B]", [Acct, CommCnt]),

    % NOTE: The `CommCnt' for the account has the NEW count,
    %       while the ranking tree has the OLD count (NEW - 1)
    if  CommCnt < ?MIN_COMMS_COUNT ->
            % Player activity continues, but hasn't hit our processing threshold
             Ranking;

        CommCnt  =:= ?MIN_COMMS_COUNT ->
            % Passing activity threshold: add user to min-count node
            % NOTE: handling this special case is faster
            %       because we already know the node exists
            MinAccts = gb_trees:get(?MIN_COMMS_COUNT, Ranking),
            gb_trees:update(?MIN_COMMS_COUNT, [Acct | MinAccts], Ranking);

        true ->
            % Remove the account from the old count-node
            OldCnt     = CommCnt - 1,
            OldNode    = gb_trees:get(OldCnt, Ranking),
            MidRanking = gb_trees:update(OldCnt, lists:delete(Acct, OldNode), Ranking),

            % Add the account to the new count-node
            case gb_trees:lookup(CommCnt, MidRanking) of

                {value, CntAccts} -> gb_trees:update(CommCnt, [Acct | CntAccts], MidRanking);   % Exists
                none              -> gb_trees:insert(CommCnt, [Acct], MidRanking)               % Newbie
            end
    end.



%%--------------------------------------------------------------------
-spec plot(Tracker  :: tracker(),
           CommType :: tt | ot | rt | tm,
           AcctCnt  :: non_neg_integer(),
           Rankings :: count_tree(),
           Markers  :: [non_neg_integer()]) -> {ok, string()}
                                             | {{error, term()}, string()}.
%%
% @doc  Creates gnuplot scripts, data files and images.
%%--------------------------------------------------------------------
plot(Tracker, CommType, AcctCnt, Rankings, Markers) ->

    % TODO: Formalize file names and locations
    Name  = ?str_fmt("~s.~s.~s", [?MODULE, Tracker, CommType]),
    FPath = lists:flatten(io_lib:format("/tmp/sila/plot/~s.dat", [Name])),
    filelib:ensure_dir(FPath),
    {ok, FOut} = file:open(FPath, [write]),

    % Write out the tweet counts for each player
    io:format(FOut, "# players: trk[~s] comm[~s]~n#~n", [Tracker, CommType]),

    Ranker = fun Recur(_, ToGo) when ToGo =< 0 -> ok;
                 Recur(Ranks, ToGo) ->
                 case gb_trees:is_empty(Ranks) of
                     true  -> ok;
                     false ->
                         {Cnt, BPs, NewRanks} = gb_trees:take_largest(Ranks),

                         lists:foreach(fun(_) -> io:format(FOut, "~B~n", [Cnt]) end, BPs),
                         Recur(NewRanks, ToGo - length(BPs))
                 end end,

    MaxAccts = ?plot_num_players(Tracker),
    Ranker(Rankings, MaxAccts),
    file:close(FOut),

    CommCode = string:uppercase(atom_to_list(CommType)),
    plot:plot(#{name    => Name,
                data    => FPath,
                title   => ?str_fmt("Big Players by Tweets (~s: ~s)", [CommCode,
                                                                       twitter:to_hashtag(Tracker)]),
                dtitle  => ?str_fmt("~s tweets", [Tracker]),
                xlabel  => ?str_fmt("Players (~B of ~B)", [MaxAccts, AcctCnt]),
                ylabel  => ?str_fmt("~s Tweets", [CommCode]),
                xrange  => {0, MaxAccts},
                yrange  => {0, ?plot_num_tweets(Tracker)},
                markers => Markers}).


%%--------------------------------------------------------------------
-spec say_ontology(JVM  :: atom(),
                   Msg  :: json_binary()) -> tuple().
%%
% @doc  Sends an update for the say-sila ontology.
%
%       NOTE: Ontology updates are currently disabled.
% @end  --
say_ontology(JVM, Msg) ->
    WorkRef = make_ref(),
    ?warning("Ontology disabled: jvm[~s] ref~p msg[~s]", [JVM, WorkRef, Msg]),
    {say, JVM} ! {self(), WorkRef, sila, Msg}.
    
