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

%%--------------------------------------------------------------------
-type words()       :: [binary()].
-type count_tree()  :: gb_trees:tree(integer(), list()).
%type count_trees() :: [{atom(), count_tree()}].    % proplist[tt, rt, tm]  % FIXME: <<DELETE!


%%--------------------------------------------------------------------
-record(counts, {tt = 0 :: non_neg_integer() | count_tree(),    % Num original tweets
                 rt = 0 :: non_neg_integer() | count_tree(),    % Num times retweeted
                 tm = 0 :: non_neg_integer() | count_tree()}).  % Num times mentioned
-type counts() :: #counts{}.


%%--------------------------------------------------------------------
-record(state, {tracker     :: atom(),
                players     :: map(),
                rankings    :: counts(),
                tweet_total :: integer() }).   % number of tweets processed
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
handle_cast({tweet, Tweet = #tweet{screen_name = Acct,
                                   type        = _Type}},
            State = #state{players     = Players,
                           rankings    = Rankings,
                           tweet_total = Total}) ->

    %?debug("Tweet: acct[~s] type[~s]", [Acct, _Type]),
    %
    % Update the tweeter's counter for this tweet
    % TODO: differentiate between original and retweets
    Counts = maps:get(Acct, Players, #counts{}),
    NumTTs = 1 + Counts#counts.tt,

    MidPlayers = maps:put(Acct, Counts#counts{tt = NumTTs}, Players),
    NewRanksTT = update_ranking(Acct, #counts.tt, MidPlayers, Rankings#counts.tt),

    % And update the social network counts/rankings
    {NewPlayers,
     NewRanksRT,
     NewRanksTM} = check_network(Tweet,
                                 MidPlayers,
                                 Rankings#counts.rt,
                                 Rankings#counts.tm),

     NewRankings = #counts{tt = NewRanksTT,
                           rt = NewRanksRT,
                           tm = NewRanksTM},

    {noreply, State#state{players     = NewPlayers,
                          rankings    = NewRankings,
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
    ReRanker = gb_trees:insert(?MIN_COUNT, [], gb_trees:empty()),
    Rankings = #counts{tt = ReRanker,
                       rt = ReRanker,
                       tm = ReRanker},

    #state{tracker     = Tracker,
           players     = #{},
           rankings    = Rankings,
           tweet_total = 0}.



%%--------------------------------------------------------------------
-spec check_network(Tweet   :: tweet(),
                    Players :: map(),
                    RanksRT :: count_tree(),
                    RanksTM :: count_tree()) -> {map(), count_tree(), count_tree()}.
%%
% @doc  Updates a `Players' map according to the retweet/mention counts
%       in the specified `Tweet'.
% @end  --
check_network(Tweet = #tweet{screen_name = Acct,
                             text        = Text},
              Players,
              RanksRT,
              RanksTM) ->
    Words = string:split(Text, " ", all),

    {MidWords,
     MidPlayers,
     NewRanksRT} = check_retweet(Tweet, Words, Players, RanksRT),

    {NewPlayers,
     NewRanksTM} = check_mentions(Acct, MidWords, MidPlayers, RanksTM),

    {NewPlayers, NewRanksRT, NewRanksTM}.



%%--------------------------------------------------------------------
-spec check_retweet(Tweet   :: tweet(),
                    Words   :: words(),
                    Players :: map(),
                    Ranking :: count_tree()) -> {words(), map(), count_tree()}.

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
              Ranking) ->
    %
    % Pull off the colon from the author reference to verify the retweeted author
    {NewPlayers,
     NewRanking} = case binary:split(AuthRef, <<":">>, [trim]) of

        [Author] ->
            AuthCnts = maps:get(Author, Players, #counts{}),
            NumRTs   = 1 + AuthCnts#counts.rt,
            ?debug("Reweet: auth[~s] cnt[~B] acct[~s]", [Author, NumRTs, Acct]),

            % Updates the retweeted author's (NOT the tweeter's) counts/ranking
            MidPlayers = maps:put(Author, AuthCnts#counts{rt = NumRTs}, Players),
            {MidPlayers,
             update_ranking(Author, #counts.rt, MidPlayers, Ranking)};

        [Who] ->
            ?warning("Cannot verify retweet: id[~s] acct[~s] auth[~p =/= ~p]",
                     [ID, Acct, Author, Who]),
            {Players, Ranking}
    end,
    {RestWords, NewPlayers, NewRanking};


check_retweet(#tweet{type           = retweet,
                     id             = ID,
                     rt_screen_name = Author,
                     screen_name    = Acct},
              Words,
              Players,
              Ranking) ->
    ?warning("Non-standard retweet text: id[~p] acct[~s] auth[~s]", [ID, Acct, Author]),
    {Words, Players, Ranking};


check_retweet(_, Words, Players, Ranking) ->
    % Ignore non-retweets
    {Words, Players, Ranking}.



%%--------------------------------------------------------------------
-spec check_mentions(Acct    :: binary(),
                     Words   :: words(),
                     Players :: map(),
                     Ranking :: count_tree()) -> {map(), count_tree()}.
%%
% @doc  Updates the `Players' map and the mentions (`tm') `Rankings'
%       with respect to the accounts mentioned in the word list from
%       the tweet text.
% @end  --
check_mentions(_, [], Players, Ranking) ->
    {Players, Ranking};


check_mentions(Acct, [<<$@, Mention/binary>> | RestWords], Players, Ranking) ->
    %
    % Someone was mentioned, update her count
    Counts = maps:get(Mention, Players, #counts{}),
    NumTMs = 1 + Counts#counts.tm,
    ?debug("Mention: tm[~s] cnt[~B] acct[~s]", [Mention, NumTMs, Acct]),

    % Updates the mentioned account's (NOT the tweeter's) counts/ranking
    NewPlayers = maps:put(Mention, Counts#counts{tm = NumTMs}, Players),
    check_mentions(Acct,
                   RestWords,
                   NewPlayers,
                   update_ranking(Mention, #counts.tm, NewPlayers, Ranking));


check_mentions(Acct, [ _ | RestWords], Players, Ranking) ->
    % Normal word
    check_mentions(Acct, RestWords, Players, Ranking).



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
    case maps:get(Acct, Players) of

        Counts when element(Counter, Counts) < (?MIN_COUNT-1) ->
            % Player activity continues, but hasn't hit our processing threshold
             Ranking;

        Counts when element(Counter, Counts) =:= (?MIN_COUNT-1) ->
            % Passing activity threshold: add user to min-count node
            % NOTE: handling this special case is faster
            %       because we already know the node exists
            MinAccts = gb_trees:get(?MIN_COUNT, Ranking),
            gb_trees:update(?MIN_COUNT, [Acct | MinAccts], Ranking);

        Counts ->
            % Remove the account from the old count-node
            AcctCnt    = element(Counter, Counts),
            OldNode    = gb_trees:get(AcctCnt, Ranking),
            MidRanking = gb_trees:update(AcctCnt, lists:delete(Acct, OldNode), Ranking),

            % Add the account to the new count-node
            case gb_trees:lookup(AcctCnt, MidRanking) of

                {value, CntAccts} -> gb_trees:update(AcctCnt, [Acct | CntAccts], MidRanking);   % Exists
                none              -> gb_trees:insert(AcctCnt, [Acct], MidRanking)               % Newbie
            end
    end.
