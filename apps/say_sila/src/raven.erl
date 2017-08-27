%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc The "Say Sila" Raven Twitter module. This module provides a
%%      high level look at the emotions surrounding Climate Change
%%      using Twitter.
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(raven).
-behaviour(gen_server).


-export([start_link/0, stop/0,
         connect/0,
         get_big_players/2,
         get_big_tweets/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("twitter.hrl").

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



%%--------------------------------------------------------------------
-spec connect() -> ok.
%%
% @doc  Higher level connection functionality for Twitter
% @end  --
connect() ->
    gen_server:cast(?MODULE, authenticate).



%%--------------------------------------------------------------------
-spec get_big_players(Tracker :: atom()
                               | players(),
                      BigP100 :: float()) -> {big_players(), players()}.
%%
% @doc  Gets the players for the Twitter tracking code (`cc' or `gw')
%       and partitions them into two lists: the "big players", who
%       form `BigP100' percent of the tweet communications, and
%       the rest of the players.
%
%       The function will also accept a list of players, rather than
%       a tracking code.  In this case the call to the database is skipped
%       and the player list is used in its place.  (This functionality is
%       primarily for debug purposes, and the function will expect that
%       the players are ordered by descending tweet count.)
%
%       The function returns a triple containing an adjusted big player
%       tweet count percentage, a list of the big players, and a list of
%       the regular players.
%
%       NOTE: `BigP100' must be between 0.0 (inclusive) and 1.0 (inclusive).
% @end  --
get_big_players(Tracker, BigP100) when is_atom(Tracker) ->
    get_big_players(twitter:get_players(Tracker), BigP100);


get_big_players(Players, BigP100) when    BigP100 >= 0.0
                                  andalso BigP100 =< 1.0 ->
    TweetTotal = lists:foldl(fun(#player{tweet_cnt = Cnt}, Acc) -> Acc + Cnt end,
                             0,
                             Players),
    {AdjBigP100, BigPlayers, RegPlayers} = get_big_players_aux(BigP100, TweetTotal, Players),

    % Warn if we had to pull more tweets than requested
    if  abs(AdjBigP100 - BigP100) > 0.001 ->
            ?warning("Adjusted big-player activity percentage: ~6.3f % >> ~6.3f %",
                     [100 * BigP100,
                      100 * AdjBigP100]);
        true -> ok
    end,
    {BigPlayers, RegPlayers};


get_big_players(_, BigP100) when   is_float(BigP100)
                            orelse is_integer(BigP100) ->
    ?error("Specify percentage between 0 and 1"),
    error(badarg).



%%--------------------------------------------------------------------
-spec get_big_tweets(Tracker :: atom(),
                     BigP100 :: float()) -> {big_players(), players()}.
%%
% @doc  Gets the tweets for the Twitter tracking code (`cc' or `gw')
%       and partitions them into two lists: tweets from the "big players",
%       who contribute `BigP100' percent of the tweet communications, and
%       the tweets from the rest of the players.
%
%       The function returns a triple containing an adjusted big player
%       tweet count percentage, a list of the big players, and a list of
%       the regular players.
%
%       NOTE: `BigP100' must be between 0.0 (inclusive) and 1.0 (inclusive).
% @end  --
get_big_tweets(Tracker, BigP100) ->
    {BigPlayers, RegPlayers} = get_big_players(Tracker, BigP100),
    BigTweets = twitter:get_tweets(Tracker, BigPlayers),
    RegTweets = twitter:get_tweets(Tracker, RegPlayers),
    weka:tweets_to_arff(io_lib:format("tweets.~s.big", [Tracker]), BigTweets),
    weka:tweets_to_arff(io_lib:format("tweets.~s.reg", [Tracker]), RegTweets).




%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(list()) -> any().
%%
% @doc  Handles placing the first twig in Raven's data nest.
% @end  --
init([go]) ->
    ?notice("The raven has taken flight"),
    process_flag(trap_exit, true),

    % Until we get a WUI, help a user out
    %gen_server:connect()

    {ok, #state{}}.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _State) ->
    ?notice("Ending the flight of the raven: why[~p]", [Why]),
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
    ?notice("Please retrieve your PIN from ~s~n", [URL]),

    % TODO: We need a proper UI for PIN entry
    %{ok, PIN} = io:fread("PIN> ", "~s"),
    %twitter:authenticate(PIN),
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
%%--------------------------------------------------------------------
-spec get_big_players_aux(BigP100  :: float(),
                          TotalCnt :: players(),
                          Players  :: players()) -> {float(), big_players(), players()}.
%%
% @doc  Partitions the into two lists: the "big players", who
%       form `BigP100' percent of the tweet communications, and
%       the rest of the players.  Since we have to split on a
%       player, the percentages may not add up, an adjusted
%       percentage is included as the first item in the returned
%       triple.
% @end  --
get_big_players_aux(BigP100, TotalCnt, Players) ->
    get_big_players_aux(BigP100, TotalCnt, Players, 0, []).



%%--------------------------------------------------------------------
-spec get_big_players_aux(BigP100    :: float(),
                          TotalCnt   :: players(),
                          Players    :: players(),
                          BigCntAcc  :: integer(),
                          BigPlayers :: players()) -> {float(), big_players(), players()}.
%%
% @doc  Partitions the into two lists: the "big players", who
%       form `BigP100' percent of the tweet communications, and
%       the rest of the players.  Since we have to split on a
%       player, the percentages may not add up, an adjusted
%       percentage is included as the first item in the returned
%       triple.
%
%       This sub-aux function takes acculators for the count of
%       big player tweets, and the growing list of big players.
% @end  --
get_big_players_aux(_, TotalCnt, [], BigCntAcc, BigPlayers) ->
    %
    % We normally shouldn't get here because this means no regular players!
    {BigCntAcc / TotalCnt, BigPlayers, []};


get_big_players_aux(BigP100, TotalCnt, [Player|Rest], BigCntAcc, BigPlayers) ->
    %
    % The current Player is considered a big player, then we use his stats
    % to see if we're done partitioning.
    NewBigCnt  = BigCntAcc + Player#player.tweet_cnt,
    AdjBigP100 = NewBigCnt / TotalCnt,
    NewBigPlayers = [Player | BigPlayers],
    case AdjBigP100 >= BigP100 of
        true  -> {AdjBigP100, lists:reverse(NewBigPlayers), Rest};
        false -> get_big_players_aux(BigP100, TotalCnt, Rest, NewBigCnt, NewBigPlayers)
    end.
