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
         get_biggies/2,
         get_biggies/3,
         get_big_p100/2,
         get_big_venn/2,
         get_comm_codes/0,
         get_comm_codes/1,
         get_comm_combos/0,
         get_players/1,
         get_rankings/1,
         get_totals/1,
         ontologize/1,
         plot/2,
         plot/3,
         reset/1,
         tweet/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("player.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(MODULES,    #{cc => player_cc,
                      gw => player_gw}).
-define(reg(Key), maps:get(Key, ?MODULES, ?MODULE)).

%% Plotting definitions
-define(PLOT_MARKERS,           [0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.50]).
-define(PLOT_BP_RATES,          [0.0025, 0.0050, 0.0075, 0.0100, 0.0125, 0.0150, 0.0175, 0.0200]).
-define(PLOT_MIN_DECEL,         0.0008).
-define(PLOT_NUM_PLAYERS,       #{cc => 300,
                                  gw => 300}).
-define(plot_num_players(Trk),  maps:get(Trk, ?PLOT_NUM_PLAYERS)).

-define(PLOT_NUM_TWEETS,        #{cc => 16000,
                                  gw =>  1500}).
-define(plot_num_tweets(Trk),   maps:get(Trk, ?PLOT_NUM_TWEETS)).

%% Record handling
-define(COUNTS,             record_info(fields, counts)).
-define(counts(),           lists:zip(?COUNTS, lists:seq(2, record_info(size, counts)))).

-define(NEW_PROFILE,        #profile{cnts = #counts{},
                                     emos = emo:stoic(0)}).


%%--------------------------------------------------------------------
-record(state, {tracker     :: atom(),
                players     :: map(),
                rankings    :: counts(),
                totals      :: counts(),    % number of tt|ot|rt|tm processed
                jvm_node    :: atom() }).
-type state() :: #state{}.
-type words() :: [binary()].



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: atom()) -> {ok, pid()}
                                     |  ignore
                                     |  {error, term()}.
%%
% @doc  Startup function for Twitter account services.
% @end  --
start_link(Tracker) ->
    gen_server:start_link({?REG_DIST, ?reg(Tracker)}, ?MODULE, [Tracker], []).



%%--------------------------------------------------------------------
-spec stop(Tracker :: atom()) -> ok.
%%
% @doc  Shutdown function for account services.
% @end  --
stop(Tracker) ->
    gen_server:call(?reg(Tracker), stop).



%%--------------------------------------------------------------------
-spec get_big_p100(Tracker :: atom(),
                   BigP100 :: float()) -> proplist().
%%
% @doc  Returns top P100 percent
%
% @deprecated   The flat percentage strategy is does not provide
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
    [counts | Totals] = tuple_to_list(get_totals(Tracker)),
    [counts | Ranks ] = tuple_to_list(get_rankings(Tracker)),

    % Figure out the counts which represent the desired percentage
    GoalFlds = ?COUNTS,
    GoalCnts = [round(BigP100 * Tot) || Tot <- Totals],
    ?info("Big P100 goals: pct[~4.1f%] cnts~p", [100 * BigP100,
                                                lists:zip(GoalFlds, GoalCnts)]),

    % Pull the accounts which get us as close as we can to our goal counts
    BigActivity  = lists:map(fun get_top/1, lists:zip(GoalCnts, Ranks)),
    ActivityP100 = [erlang:insert_element(1, Act, Cnt/Tot) || {Act = {Cnt, _ }, Tot} <- lists:zip(BigActivity,
                                                                                                  Totals)],
    % Return the results as a proplist of the count types
    lists:zip(GoalFlds,  ActivityP100).



%%--------------------------------------------------------------------
-spec get_big_venn(Tracker :: atom(),
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
                    Accts = lists:map(fun(C) -> proplists:get_value(C, AcctSets) end, Comms),
                    ISet  = sets:intersection(Accts),
                    %?debug("INTERSECTION~p => ~p", [Comms, sets:to_list(ISet)]),
                    {Comms, sets:size(ISet)}
                    end,

    % Make it so...
    [Intersect(Comms) || Comms <- get_comm_combos()].



%%--------------------------------------------------------------------
-spec get_biggies(Tracker :: atom(),
                  MinRate :: float()) -> proplist().
%%
% @doc  Returns big players, candidate influencers
% @end  --
get_biggies(Tracker, MinRate) ->
    get_biggies(Tracker, MinRate, ?PLOT_MIN_DECEL).



%%--------------------------------------------------------------------
-spec get_biggies(Tracker  :: atom(),
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
    [counts | Totals] = tuple_to_list(get_totals(Tracker)),
    [counts | Ranks ] = tuple_to_list(get_rankings(Tracker)),

    % Pull the accounts which are adding tweets at or above the minimum rate
    GoalFlds = ?COUNTS,
    BigActivity  = lists:map(fun(TR) -> pull_biggies(MinRate, MinDecel, TR) end,
                             lists:zip(Totals, Ranks)),
    ActivityP100 = [erlang:insert_element(1, Act, Cnt/Tot) || {Act = {Cnt, _ }, Tot} <- lists:zip(BigActivity,
                                                                                                  Totals)],
    % Return the results as a proplist of the count types
    lists:zip(GoalFlds,  ActivityP100).



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
    get_comm_combos(N, ?COMM_CODES).



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
-spec get_players(Tracker :: atom()) -> map().
%%
% @doc  Returns the server's internal players map.
%%--------------------------------------------------------------------
get_players(Tracker) ->
    gen_server:call(?reg(Tracker), get_players).



%%--------------------------------------------------------------------
-spec get_rankings(Tracker :: atom()) -> counts().
%%
% @doc  Returns the server's internal count rankings.
%%--------------------------------------------------------------------
get_rankings(Tracker) ->
    gen_server:call(?reg(Tracker), get_rankings).



%%--------------------------------------------------------------------
-spec get_totals(Tracker :: atom()) -> counts().
%%
% @doc  Returns the server totals in a `counts' record:
%           - `tt'  total count of all tweets sent
%           - `tt'  number of retweeted messages
%           - `tm'  number of times users are mentioned
%%--------------------------------------------------------------------
get_totals(Tracker) ->
    gen_server:call(?reg(Tracker), get_totals).



%%--------------------------------------------------------------------
-spec ontologize(Tracker :: atom()) -> ok.
%%
% @doc  Finalizes ontology creation by sending post counts for the
%       original tweeters.  (Maybe there'll be more to come...)
% @end  --
ontologize(Tracker) ->
    gen_server:call(?reg(Tracker), ontologize).



%%--------------------------------------------------------------------
-spec plot(Tracker  :: atom(),
           MinDecel :: float()) -> ok.
%%
% @doc  Creates gnuplot scripts, data files and images.
% @end  --
plot(Tracker, MinDecel) ->
    plot(Tracker, ?PLOT_BP_RATES, MinDecel).



%%--------------------------------------------------------------------
-spec plot(Tracker  :: atom(),
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

    % Draw lines on the plot at certain percentile points
    Marker = fun(Comm) ->
                 InfoByComm = [proplists:get_value(Comm, InfoByRate) || InfoByRate <- Biggies],
                 [length(BPs) || {_Pct, _Cnt, BPs} <- InfoByComm]
                 end,

    Plotter  = fun({Comm, Ndx}) ->
                   % How many accounts participated in the current communication mode?
                   Accounter = fun(_, #profile{cnts = Cnts}, Acc) ->
                                   Cnt = element(Ndx, Cnts),
                                   case  Cnt > 0 of
                                       true  -> Acc + 1;
                                       false -> Acc
                                    end end,
                   AcctCnt = maps:fold(Accounter, 0, Players),
                   Ranks   = element(Ndx, Rankings),
                   Markers = Marker(Comm),
                   plot(Tracker, Comm, AcctCnt, Ranks, Markers)
                   end,
    lists:foreach(Plotter, ?counts()).



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
    {reply, ok, reset_state(Tracker)};


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
    TreeItr = gb_trees:iterator(Rankings#counts.tt),
    GBRunner(gb_trees:next(TreeItr)),
    {reply, ok, State};


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
handle_cast({tweet, Tweet = #tweet{screen_name = ScreenName,
                                   emotions    = Emos,
                                   type        = Type}},
            State = #state{players  = Players,
                           rankings = Rankings,
                           totals   = Totals,
                           jvm_node = JVM}) ->

    Acct = string:lowercase(ScreenName),
    ?info("TWEET: acct[~s] type[~s] id[~s]", [Acct, Type, Tweet#tweet.id]),

    % Update the tweet counter(s) and rank(s) for this tweet
    {MidPlayers,
     MidRankings,
     MidTotals} = case Type of
        % An original tweet ( `ot' ) implies a tweet sent ( `tt' )
        tweet ->
            MidProfMap = update_players(Acct, [?prop_counts(tt), ?prop_counts(ot)], Emos, Players),
            {MidProfMap,
             Rankings#counts{tt = update_ranking(Acct, #counts.tt, MidProfMap, Rankings#counts.tt),
                             ot = update_ranking(Acct, #counts.ot, MidProfMap, Rankings#counts.ot)},
             Totals  #counts{tt = 1 + Totals#counts.tt,
                             ot = 1 + Totals#counts.ot}};

        % Retweets are not original, just do the send ( `tt' )
        retweet ->
            MidProfMap = update_players(Acct, ?prop_counts(tt), Emos, Players),
            {MidProfMap,
             Rankings#counts{tt = update_ranking(Acct, #counts.tt, MidProfMap, Rankings#counts.tt)},
             Totals  #counts{tt = 1 + Totals#counts.tt}}
    end,

    % And update the social network counts/rankings
    {NewPlayers,
     NewRankings,
     NewTotals} = check_network(Tweet, JVM, MidPlayers, MidRankings, MidTotals),

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
-spec reset_state(Tracker :: atom()) -> state().
%%
% @doc  Process out-of-band messages
% @end  --
reset_state(Tracker) ->

    % Grab what we need from the configuration
    {ok, App} = application:get_application(),
    JVM       = application:get_env(App, jvm_node, undefined),

    % Start the tweet tree with a node for minimum-activity players
    ReRanker = gb_trees:insert(?MIN_COMMS_COUNT, [], gb_trees:empty()),
    Rankings = #counts{tt = ReRanker,
                       ot = ReRanker,
                       rt = ReRanker,
                       tm = ReRanker},

    #state{tracker  = Tracker,
           players  = #{},
           rankings = Rankings,
           totals   = #counts{},
           jvm_node = JVM}.



%%--------------------------------------------------------------------
-spec get_top({GoalCnt :: non_neg_integer(),
               Ranking :: count_tree()}) -> {non_neg_integer(), list()}.
%%
% @doc  Attempts to get the accounts with a total as close to the goal
%       as possible.
% @end  --
get_top({GoalCnt, Ranking}) ->
    get_top(GoalCnt, Ranking, 0, []).



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
-spec pull_biggies(MinRate  :: float(),
                   MinDecel :: float(),
                   GrpData  :: {non_neg_integer(),
                                count_tree()}) -> {non_neg_integer(), list()}.
%%
% @doc  Attempts to get the accounts with a total as close to the goal
%       as possible.
% @end  --
pull_biggies(MinRate, MinDecel, {Total, Ranking}) ->
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
                            ?debug("Pulling biggies: rate[~6.4f] decel[~6.4f] cnt[~B => ~B] tree[~B => ~B]",
                                   [Rate, Decel, CountIn, Count, gb_trees:size(RanksIn), gb_trees:size(Ranks)]),

                            pull_biggies(MinRate, MinDecel, Total, Tally, Ranks, Count, NewAccts);
                        false ->
                            % This group didn't make the cut.  We're done!
                            {CountIn, AcctsIn}
                    end
            end
    end.



%%--------------------------------------------------------------------
-spec update_players(Account   :: binary(),
                     CommCodes :: proplist() | property(),
                     TweetEmos :: emotions(),
                     Players   :: map()) -> map().
%%
% @doc  Retrieves a player's `profile' from the `Players' map, updates it
%       with respect to the specified `Tweet', and returns the new map.
% @end  --
update_players(Account, CommCode, TweetEmos, Players) when is_tuple(CommCode) ->
    %
    update_players(Account, [CommCode], TweetEmos, Players);


update_players(Account, CommCodes, TweetEmos, Players) ->
    %
    Profile  = maps:get(Account, Players, ?NEW_PROFILE),
    AcctEmos = Profile#profile.emos,

    %?debug("Player EMOS: ~p", [AcctEmos]),
    %?debug("Tweet  EMOS: ~p", [TweetEmos]),

    NewEmos = emo:average(AcctEmos, TweetEmos),


    update_players_aux(Profile#profile{emos = NewEmos}, Account, CommCodes, Players).



%%--------------------------------------------------------------------
-spec update_players(Account   :: binary(),
                     CommCodes :: proplist() | property(),
                     Players   :: map()) -> map().
%%
% @doc  Retrieves a player's `profile' from the `Players' map, updates it
%       for the specified communication type, and returns the new map.
% @end  --
update_players(Account, CommCode, Players) when is_tuple(CommCode) ->
    %
    update_players(Account, [CommCode], Players);


update_players(Account, CommCodes, Players) ->
    %
    update_players_aux(maps:get(Account, Players, ?NEW_PROFILE),
                       Account, CommCodes, Players).



%%--------------------------------------------------------------------
-spec update_players_aux(Profile   :: profile(),
                         Account   :: binary(),
                         CommCodes :: pos_integer()
                                    | proplist(),
                         Players   :: map()) -> map().
%%
% @doc  Updates the player's `profile' om the `Players' map for the
%       specified communication type and returns a new map.
% @end  --
update_players_aux(Profile, Account, CommCodes, Players) ->
    %
    ReCounter = fun({Comm, Elm}, Counts) ->
                    NewVal = 1 + element(Elm, Counts),
                    ?debug("Update: ~s[~B] acct[~s]", [Comm, NewVal, Account]),

                    setelement(Elm, Counts, NewVal)
                    end,

    NewCounts = lists:foldl(ReCounter, Profile#profile.cnts, CommCodes),

    maps:put(Account, Profile#profile{cnts = NewCounts}, Players).



%%--------------------------------------------------------------------
-spec check_network(Tweet    :: tweet(),
                    JVM      :: atom(),
                    Players  :: map(),
                    Rankings :: counts(),
                    Totals   :: counts()) -> {map(), counts(), counts()}.
%%
% @doc  Updates a `Players' map according to the retweet/mention counts
%       in the specified `Tweet'.
% @end  --
check_network(Tweet = #tweet{text = Text},
              JVM,
              Players,
              Rankings = #counts{rt = RanksRT, tm = RanksTM},
              Totals   = #counts{rt = TotalRT, tm = TotalTM}) ->

    Words = string:split(Text, " ", all),

    {MidWords,
     MidPlayers,
     NewRanksRT,
     NewTotalRT} = check_retweet(Tweet, Words, JVM, Players, RanksRT, TotalRT),

    {NewPlayers,
     NewRanksTM,
     NewTotalTM} = check_mentions(Tweet, MidWords, JVM, MidPlayers, RanksTM, TotalTM),

    {NewPlayers,
     Rankings#counts{rt = NewRanksRT, tm = NewRanksTM},
     Totals  #counts{rt = NewTotalRT, tm = NewTotalTM}}.



%%--------------------------------------------------------------------
-spec check_retweet(Tweet   :: tweet(),
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
check_retweet(Tweet = #tweet{type           = retweet,
                             id             = ID,
                             rt_screen_name = Author,
                             screen_name    = Acct},
              [<<"RT">>, <<$@, AuthRef/binary>> | RestWords],
              JVM,
              Players,
              Ranking,
              Total) ->
    %
    % Pull off the colon from the author reference to verify the retweeted author
    {NewPlayers,
     NewRanking,
     NewTotal} = case binary:split(AuthRef, <<":">>, [trim]) of

        [Author] ->
            % Updates the retweeted author's (NOT the tweeter's) counts/ranking
            MidPlayers = update_players(Author, ?prop_counts(rt), Players),

            % Inform the say-sila ontology about the retweet
            say_ontology(JVM, twitter:ontologize(Tweet, json)),

            {MidPlayers,
             update_ranking(Author, #counts.rt, MidPlayers, Ranking),
             1 + Total};

        [Who] ->
            ?warning("Cannot verify retweet: id[~s] acct[~s] auth[~p =/= ~p]",
                     [ID, Acct, Author, Who]),
            {Players, Ranking, Total}
    end,
    {RestWords, NewPlayers, NewRanking, NewTotal};


check_retweet(#tweet{type           = retweet,
                     id             = ID,
                     rt_screen_name = Author,
                     screen_name    = Acct}, Words, _, Players, Ranking, Total) ->
    %
    ?warning("Non-standard retweet text: id[~p] acct[~s] auth[~s]", [ID, Acct, Author]),
    {Words, Players, Ranking, Total};


check_retweet(_, Words, _, Players, Ranking, Total) ->
    % Ignore non-retweets
    {Words, Players, Ranking, Total}.



%%--------------------------------------------------------------------
-spec check_mentions(Tweet   :: tweet(),
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
check_mentions(_, [], _, Players, Ranking, Total) ->
    {Players, Ranking, Total};


check_mentions(Tweet, [<<$@>> | RestWords], JVM, Players, Ranking, Total) ->
    %
    % Skip lone @-sign
    check_mentions(Tweet, RestWords, JVM, Players, Ranking, Total);


check_mentions(Tweet = #tweet{id          = ID,
                              screen_name = Acct},
               [<<$@, Mention/binary>> | RestWords],
               JVM,
               Players,
               Ranking,
               Total) ->
    % Updates the mentioned account's (NOT the tweeter's) counts/ranking
    NewPlayers = update_players(Mention, ?prop_counts(tm), Players),

    % Inform the say-sila ontology about the mention
    TwID = list_to_binary(?str_fmt("t~s", [ID])),               % Prefix for Clj-var
    Roles = [#{domain => Acct, property => makesMentionIn, range => TwID},
             #{domain => TwID, property => hasMentionOf,   range => Mention}],
    say_ontology(JVM, jsx:encode(Roles)),

    % Look for more mentions in the rest of the tweet text
    check_mentions(Tweet,
                   RestWords,
                   JVM,
                   NewPlayers,
                   update_ranking(Mention, #counts.tm, NewPlayers, Ranking),
                   1 + Total);


check_mentions(Tweet, [ _ | RestWords], JVM, Players, Ranking, Total) ->
    % Normal word
    check_mentions(Tweet, RestWords, JVM, Players, Ranking, Total).



%%--------------------------------------------------------------------
-spec update_ranking(Acct    :: binary(),
                     Counter :: pos_integer(),
                     Players :: map(),
                     Ranking :: count_tree()) -> count_tree().
%%
% @doc  Updates the account's ranking in the count tree.
%
%       NOTE: We assume the `Account' exists in the `Players' maps,
%             so update `Players' before calling this function.
% @end  --
update_ranking(Acct, Counter, Players, Ranking) ->
    %
    %?debug("Ranking counter: ~B", [Counter]),

    % NOTE: The players map  has the NEW count, while
    %       the ranking tree has the OLD count (NEW - 1)
    case maps:get(Acct, Players) of

        #profile{cnts = Counts} when element(Counter, Counts) < (?MIN_COMMS_COUNT) ->
            % Player activity continues, but hasn't hit our processing threshold
             Ranking;

        #profile{cnts = Counts} when element(Counter, Counts) =:= (?MIN_COMMS_COUNT) ->
            % Passing activity threshold: add user to min-count node
            % NOTE: handling this special case is faster
            %       because we already know the node exists
            MinAccts = gb_trees:get(?MIN_COMMS_COUNT, Ranking),
            gb_trees:update(?MIN_COMMS_COUNT, [Acct | MinAccts], Ranking);

        Profile ->
            % Remove the account from the old count-node
            AcctCnt    = element(Counter, Profile#profile.cnts),
            OldCnt     = AcctCnt - 1,
            OldNode    = gb_trees:get(OldCnt, Ranking),
            MidRanking = gb_trees:update(OldCnt, lists:delete(Acct, OldNode), Ranking),

            % Add the account to the new count-node
            case gb_trees:lookup(AcctCnt, MidRanking) of

                {value, CntAccts} -> gb_trees:update(AcctCnt, [Acct | CntAccts], MidRanking);   % Exists
                none              -> gb_trees:insert(AcctCnt, [Acct], MidRanking)               % Newbie
            end
    end.



%%--------------------------------------------------------------------
-spec plot(Tracker  :: atom(),
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
                   Msg  :: json_binary()) -> ok.
%%
% @doc  Sends an update for the say-sila ontology.
%
%       NOTE: Ontology updates are currently disabled.
% @end  --
say_ontology(JVM, Msg) ->
    WorkRef = make_ref(),
    ?debug("Ontology disabled: jvm[~s] ref~p msg[~s]", [JVM, WorkRef, Msg]).
    %{say, JVM} ! {self(), WorkRef, sila, Msg}.
    
