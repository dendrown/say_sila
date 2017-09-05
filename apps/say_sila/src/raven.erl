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

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/0, stop/0,
         connect/0,
         get_big_players/2,
         get_big_tweets/2,
         run_tweet_csv/1]).     %% DEBUG!
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("twitter.hrl").

-include_lib("ecsv/include/ecsv.hrl").

-record(state, {weka_node :: string() }).
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
    {ok, App} = application:get_application(),
    WekaNode = application:get_env(App, weka_node, undefined),
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, [WekaNode], []).



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
get_big_players(_, BigP100) when   BigP100 =< 0.0
                            orelse BigP100 >= 1.0 ->
    % Give the user a break and don't make them wait on a long query
    % before telling them what we want a percentage value to look like.
    ?error("Specify percentage between 0 and 1"),
    error(badarg);


get_big_players(Tracker, BigP100) when is_atom(Tracker) ->
    get_big_players(twitter:get_players(Tracker), BigP100);


get_big_players(Players, BigP100) ->
    TweetTotal = lists:foldl(fun(#player{tweet_cnt = Cnt}, Acc) -> Acc + Cnt end,
                             0,
                             Players),
    {AdjBigP100, BigPlayers, RegPlayers} = get_big_players_aux(BigP100, TweetTotal, Players),

    % Warn if we had to pull more tweets than requested
    if  abs(AdjBigP100 - BigP100) > 0.001 ->
            ?warning("Adjusted big-player activity percentage: ~6.3f% >> ~6.3f%",
                     [100 * BigP100,
                      100 * AdjBigP100]);
        true -> ok
    end,
    {BigPlayers, RegPlayers}.



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
    %
    % FIXME: The name of this function doesn't represent its usage very well
    %
    ?notice("Preparing big-vs-regular player tweets"),
    {BigPlayers,
     RegPlayers} = get_big_players(Tracker, BigP100),
    ?info("Player counts: big[~B] reg[~B]", [length(BigPlayers), length(RegPlayers)]),

    ActivityPct = round(100 * BigP100),
    lists:foreach(fun({Size, Player}) ->

                      ?debug("Pulling ~s-player tweets", [Size]),
                      Tweets = twitter:get_tweets(Tracker, Player),

                      ?debug("Packaging tweets for Weka"),
                      FStub = io_lib:format("tweets.~s.~B.~s", [Tracker, ActivityPct, Size]),
                      {ok, FPath} = weka:tweets_to_arff(FStub, Tweets),
                      {weka, jvm@chiron} ! {self(), emote, FPath}
                      end,
                  [{big, BigPlayers}, {reg, RegPlayers}]).





%%--------------------------------------------------------------------
-spec run_tweet_csv(FName :: string()) -> {ok, integer()}.
%%
% @doc  Formats and prints a Weka output CSV.
%
%       NOTE: This is a test/debug function that will likely disappear.
%%--------------------------------------------------------------------
run_tweet_csv(FName) ->
    Separator = "-----------------------------------------------------~n",
    LineFn = fun({newline, [ID, ScreenName, Anger, Fear, Sadness, Joy]}, Cnt) ->
                 io:format(Separator),
                 io:format("   tweet id : ~s~n", [ID]),
                 io:format("screen name : ~s~n", [ScreenName]),
                 io:format("      anger : ~s~n", [Anger]),
                 io:format("       fear : ~s~n", [Fear]),
                 io:format("    sadness : ~s~n", [Sadness]),
                 io:format("        joy : ~s~n", [Joy]),
                 Cnt + 1;
                ({eof}, Cnt) -> Cnt end,
    {ok, In} = file:open(FName, [read]),
    Result   = ecsv:process_csv_file_with(In, LineFn, 0, #ecsv_opts{quote=$'}),
    io:format(Separator),
    file:close(In),
    Result.



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(list()) -> any().
%%
% @doc  Handles placing the first twig in Raven's data nest.
% @end  --
init([WekaNode]) ->
    ?notice("The raven has taken flight: weka[~s]", [WekaNode]),
    process_flag(trap_exit, true),

    {ok, #state{weka_node = WekaNode}}.



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
