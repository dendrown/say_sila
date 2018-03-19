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

-export([start_link/1, stop/1, dbg/0,
         get_big_p100/2,
         get_players/1,
         get_rankings/1,
         get_totals/1,
         plot/1,
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

-define(COUNTS,             record_info(fields, counts)).
-define(counts(),           lists:zip(?COUNTS, lists:seq(2, record_info(size, counts)))).
-define(PLOT_NUM_PLAYERS,   300).
-define(PLOT_NUM_TWEETS,    1500).
-define(PLOT_MARKERS,       [0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.45, 0.50]).
-define(NEW_PROFILE,        #profile{cnts = #counts{},
                                     emos = emo:stoic(0)}).


%%--------------------------------------------------------------------
-record(state, {tracker     :: atom(),
                players     :: map(),
                rankings    :: counts(),
                totals      :: counts() }).   % number of tt|ot|rt|tm processed
-type state() :: #state{}.
-type words() :: [binary()].


% FIXME: delete me
dbg() -> ?counts().

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
%%--------------------------------------------------------------------
get_big_p100(_, BigP100) when   BigP100 =< 0.0
                         orelse BigP100 >= 1.0 ->
    % before telling them what we want a percentage value to look like.
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
-spec plot(Tracker :: atom()) -> ok.
%%
% @doc  Creates gnuplot scripts, data files and images.
%%--------------------------------------------------------------------
plot(Tracker) ->

    Players  = get_players(Tracker),
    AcctCnt  = maps:size(Players),
    Rankings = get_rankings(Tracker),
    BigP100s = [get_big_p100(Tracker, Pct) || Pct <- ?PLOT_MARKERS],

    % Draw lines on the plot at certain percentile points
    Marker = fun(Comm) ->
                 Bigs = [proplists:get_value(Comm, Pct) || Pct <- BigP100s],
                 [length(BPs) || {_,_,BPs} <- Bigs]
                 end,

    Plotter  = fun({Comm, Ndx}) ->
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
handle_call(get_players, _From, State = #state{players = Players}) ->
    {reply, Players, State};


handle_call(get_rankings, _From, State = #state{rankings = Rankings}) ->
    {reply, Rankings, State};


handle_call(get_totals, _From, State = #state{totals = Totals}) ->
    {reply, Totals, State};


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
handle_cast({tweet, Tweet = #tweet{screen_name = Acct,
                                   emotions    = Emos,
                                   type        = Type}}, State = #state{players  = Players,
                                                                        rankings = Rankings,
                                                                        totals   = Totals}) ->

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
                             ot = 1 + Totals#counts.tt}};

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
     NewTotals} = check_network(Tweet, MidPlayers, MidRankings, MidTotals),

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
-spec reset_state(Tracker :: atom()) -> state().
%%
% @doc  Process out-of-band messages
% @end  --
reset_state(Tracker) ->
    %
    % Start the tweet tree with a node for minimum-activity players
    ReRanker = gb_trees:insert(?MIN_COMMS_COUNT, [], gb_trees:empty()),
    Rankings = #counts{tt = ReRanker,
                       ot = ReRanker,
                       rt = ReRanker,
                       tm = ReRanker},

    #state{tracker  = Tracker,
           players  = #{},
           rankings = Rankings,
           totals   = #counts{}}.



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
              Cmt     :: non_neg_integer(),
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
                    Players  :: map(),
                    Rankings :: counts(),
                    Totals   :: counts()) -> {map(), counts(), counts()}.
%%
% @doc  Updates a `Players' map according to the retweet/mention counts
%       in the specified `Tweet'.
% @end  --
check_network(Tweet = #tweet{screen_name = Acct,
                             text        = Text},
              Players,
              Rankings = #counts{rt = RanksRT, tm = RanksTM},
              Totals   = #counts{rt = TotalRT, tm = TotalTM}) ->

    Words = string:split(Text, " ", all),

    {MidWords,
     MidPlayers,
     NewRanksRT,
     NewTotalRT} = check_retweet(Tweet, Words, Players, RanksRT, TotalRT),

    {NewPlayers,
     NewRanksTM,
     NewTotalTM} = check_mentions(Acct, MidWords, MidPlayers, RanksTM, TotalTM),

    {NewPlayers,
     Rankings#counts{rt = NewRanksRT, tm = NewRanksTM},
     Totals  #counts{rt = NewTotalRT, tm = NewTotalTM}}.



%%--------------------------------------------------------------------
-spec check_retweet(Tweet   :: tweet(),
                    Words   :: words(),
                    Players :: map(),
                    Ranking :: count_tree(),
                    Total   :: non_neg_integer()) -> {words(), map(), count_tree(), non_neg_integer()}.

%%
% @doc  Updates the `Players' map with respect to a retweeted author
%       if the specified `Tweet' is a `retweet'.  If it is, the first
%       two words are stripped from the word list in the return tuple.
% @end  --
check_retweet(#tweet{type           = retweet,
                     id             = ID,
                     rt_screen_name = Author,
                     screen_name    = Acct},
              [<<"RT">>, <<$@, AuthRef/binary>> | RestWords],
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
                     screen_name    = Acct}, Words, Players, Ranking, Total) ->
    %
    ?warning("Non-standard retweet text: id[~p] acct[~s] auth[~s]", [ID, Acct, Author]),
    {Words, Players, Ranking, Total};


check_retweet(_, Words, Players, Ranking, Total) ->
    % Ignore non-retweets
    {Words, Players, Ranking, Total}.



%%--------------------------------------------------------------------
-spec check_mentions(Acct    :: binary(),
                     Words   :: words(),
                     Players :: map(),
                     Ranking :: count_tree(),
                     Total   :: non_neg_integer()) -> {map(), count_tree(), non_neg_integer()}.
%%
% @doc  Updates the `Players' map and the mentions (`tm') `Rankings'
%       with respect to the accounts mentioned in the word list from
%       the tweet text.
% @end  --
check_mentions(_, [], Players, Ranking, Total) ->
    {Players, Ranking, Total};


check_mentions(Acct, [<<$@>> | RestWords], Players, Ranking, Total) ->
    %
    % Skip lone @-sign
    check_mentions(Acct, RestWords, Players, Ranking, Total);


check_mentions(Acct, [<<$@, Mention/binary>> | RestWords], Players, Ranking, Total) ->
    %
    % Updates the mentioned account's (NOT the tweeter's) counts/ranking
    NewPlayers = update_players(Mention, ?prop_counts(tm), Players),

    check_mentions(Acct,
                   RestWords,
                   NewPlayers,
                   update_ranking(Mention, #counts.tm, NewPlayers, Ranking),
                   1 + Total);


check_mentions(Acct, [ _ | RestWords], Players, Ranking, Total) ->
    % Normal word
    check_mentions(Acct, RestWords, Players, Ranking, Total).



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
    Ranker = fun Recur(_, ToGo) when ToGo =< 0 -> ok;
                 Recur(Ranks, ToGo) ->
                 case gb_trees:is_empty(Ranks) of
                     true  -> ok;
                     false ->
                         {Cnt, BPs, NewRanks} = gb_trees:take_largest(Ranks),

                         lists:foreach(fun(_) -> io:format(FOut, "~B~n", [Cnt]) end, BPs),
                         Recur(NewRanks, ToGo - length(BPs))
                 end end,

    io:format(FOut, "# players: trk[~s] comm[~s]~n#~n", [Tracker, CommType]),
    Ranker(Rankings, ?PLOT_NUM_PLAYERS),
    file:close(FOut),

    CommCode = string:uppercase(atom_to_list(CommType)),
    plot:plot(#{name    => Name,
                data    => FPath,
                title   => ?str_fmt("Big Players by Tweets (~s: ~s)", [CommCode,
                                                                       twitter:to_hashtag(Tracker)]),
                dtitle  => ?str_fmt("~s tweets", [Tracker]),
                xlabel  => ?str_fmt("Players (~B of ~B)", [?PLOT_NUM_PLAYERS, AcctCnt]),
                ylabel  => ?str_fmt("~s Tweets", [CommCode]),
                xrange  => {0, ?PLOT_NUM_PLAYERS},
                yrange  => {0, ?PLOT_NUM_TWEETS},
                markers => Markers}).

