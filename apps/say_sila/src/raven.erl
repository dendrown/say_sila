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
         emote/1,
         emote/2,
         get_big_players/2,
         get_big_players/3,
         report/1,
         run_tweet_csv/1]).     %% DEBUG!
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("raven.hrl").
-include("twitter.hrl").

-include_lib("ecsv/include/ecsv.hrl").

-define(DEFAULT_BIG_P100, 0.15).


% Slots are for keeping everything about a player category in one place
-record(tweet_slot, {category :: atom(),
                     players  :: players(),
                     tweets   :: tweets() }).
%type tweet_slot() :: #tweet_slot{}.


-record(state, {tracker           :: atom(),
                big_percent       :: float(),
                emo_report  = #{} :: map(),
                tweet_slots = #{} :: map(),
                tweet_todo  = #{} :: map(),
                weka_node         :: string() }).
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
    WekaNode = application:get_env(App, weka_node,   undefined),
    BigP100  = application:get_env(App, big_percent, ?DEFAULT_BIG_P100),
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, [BigP100, WekaNode], []).



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
%
%       TODO: This function needs to move into the handle_call/cast logic.
% @end  --
get_big_players(Tracker, BigP100) ->
    get_big_players(Tracker, BigP100, []).



%%--------------------------------------------------------------------
-spec get_big_players(Tracker   :: atom() | players(),
                      BigP100   :: float(),
                      Options   :: property() | proplist()) -> {big_players(), players()}.
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
%       If specified, the Options passed to this function simply get passed
%       as-is to the function `twitter:get_players/2'.
%
%       NOTE: `BigP100' must be between 0.0 (inclusive) and 1.0 (inclusive).
%
%       TODO: This function needs to move into the handle_call/cast logic.
% @end  --
get_big_players(_, BigP100, _) when   BigP100 =< 0.0
                               orelse BigP100 >= 1.0 ->
    % Give the user a break and don't make them wait on a long query
    % before telling them what we want a percentage value to look like.
    ?error("Specify percentage between 0 and 1"),
    error(badarg);


get_big_players(Tracker, BigP100, Options) when is_atom(Tracker) ->
    get_big_players(twitter:get_players(Tracker, Options), BigP100, Options);


get_big_players(Players, BigP100, _) ->
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
-spec emote(Tracker :: atom()) -> ok.
%%
% @doc  Process the tweets for the Twitter tracking code (`cc' or `gw').
%       We send the compiled tweets to Weka to generate word embeddings
%       and sentiment/emotion ratings per our selected lexicons.  Weka
%       will notify that it has finished its processing via a message,
%       which this module will pick up in handle_info.
%
%       This function is the current "do what we want" function for the
%       `raven' module and the `say_sila' application...but take heed:
%       we are evolving...
% @end  --
emote(Tracker) ->
    emote(Tracker, []).



%%--------------------------------------------------------------------
-spec emote(Tracker :: atom(),
            Options :: property() | proplist()) -> ok.
%%
% @doc  Process the tweets for the Twitter tracking code (`cc' or `gw').
%       We send the compiled tweets to Weka to generate word embeddings
%       and sentiment/emotion ratings per our selected lexicons.  Weka
%       will notify that it has finished its processing via a message,
%       which this module will pick up in handle_info.
%
%       Options is a property list allowing the following:
%           - `min_tweets'  Only accounts having at least this many
%                           status upates (tweets) are included.
%           - `start'       Begining datetime to start looking for players
% @end  --
emote(Tracker, Option) when   is_atom(Option)
                       orelse is_tuple(Option) ->
    emote(Tracker, [Option]);


emote(Tracker, Options) ->
    gen_server:cast(?MODULE, {emote, Tracker, Options}).



%%--------------------------------------------------------------------
-spec report(Period :: atom()) -> {ok, integer()}.
%%
% @doc  Prepares a report on tweet data (already processed by `emote')
%       for the specified time period: `hour', `day', etc.
%%--------------------------------------------------------------------
report(Period) ->
    gen_server:call(?MODULE, {report, Period}).



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
                 io:format("   tweet id : ~p~n", [ID]),
                 io:format("screen name : ~p~n", [ScreenName]),
                 io:format("      anger : ~p~n", [Anger]),
                 io:format("       fear : ~p~n", [Fear]),
                 io:format("    sadness : ~p~n", [Sadness]),
                 io:format("        joy : ~p~n", [Joy]),
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
init([BigP100, WekaNode]) ->
    ?notice("The raven is taking flight: big[~6.3f%] weka[~s]",
            [100 * BigP100,
             WekaNode]),
    process_flag(trap_exit, true),

    % The big-player percentage range is: 0.0 (inclusive) to 1.0 (inclusive).
    if
        BigP100 >= 0.0 andalso
        BigP100 =< 1.0 ->
            {ok, #state{big_percent = BigP100,
                        weka_node   = WekaNode}};
        true ->
            ?error("Big player percentage must be between 0 and 1"),
            {stop, badarg}
    end.



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
%% handle_call:
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call({report, Period}, _From, State = #state{tracker     = Tracker,
                                                    big_percent = BigP100,
                                                    tweet_slots = SlotMap}) ->
    % FIXME: this intial version is coded as a one-shot, but
    %        we will need to adjust and recalcuate automatically
    %        every hour...
    Rpts = lists:map(fun(Size) ->
                         {Size, report(Size, Period, maps:get(Size, SlotMap, []))}
                         end,
                     [big, reg]),
    RptMap = maps:from_list(Rpts),
    RptTag = io_lib:format("~s.~B", [Tracker,
                                     round(100 * BigP100)]),
    r:report_emotions(RptTag, Period, RptMap),
    {reply, {ok, RptMap}, State#state{emo_report = RptMap}};


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


handle_cast({emote, Tracker, Options}, State = #state{big_percent = BigP100,
                                                      tweet_todo  = TodoMap,
                                                      weka_node   = WekaNode}) ->
    ?notice("Preparing big-vs-regular player tweets"),
    {BigPlayers,
     RegPlayers} = get_big_players(Tracker, BigP100, Options),
    ?info("Player counts: big[~B] reg[~B]", [length(BigPlayers), length(RegPlayers)]),

    ActivityPct = round(100 * BigP100),
    NewTodo = lists:map(fun({Size, Players}) ->
                            % Pull the actual tweets for these players from the DB
                            ?debug("Pulling ~s-player tweets", [Size]),
                            Tweets = twitter:get_tweets(Tracker, Players, Options),

                            ?debug("Packaging tweets for Weka"),
                            FStub  = io_lib:format("tweets.~s.~B.~s", [Tracker, ActivityPct, Size]),
                            {ok, FPath} = weka:tweets_to_arff(FStub, Tweets),

                            % Send to Weka to apply embedding/emotion filters
                            Lookup = make_ref(),
                            {weka, WekaNode} ! {self(), Lookup, emote, FPath},
                            {Lookup, #tweet_slot{category = Size,
                                                 players  = Players,
                                                 tweets   = Tweets}}
                            end,
                        [{big, BigPlayers}, {reg, RegPlayers}]),
    NewTodoMap = maps:from_list(NewTodo),
    {noreply, State#state{tracker    = Tracker,
                          tweet_todo = maps:merge(TodoMap, NewTodoMap)}};


handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
-spec handle_info(Msg   :: term(),
                  State :: term()) -> any().
%%
% @doc  Process out-of-band messages
% @end  --
handle_info({From, Ref, emote, ArgMap = #{csv := FPathCSV}}, State = #state{tweet_slots = SlotMap,
                                                                            tweet_todo  = TodoMap}) ->
    NewState = case maps:take(Ref, TodoMap) of
        {#tweet_slot{category = Category,
                     tweets   = Tweets},
         NewTodoMap} ->
            ?notice("Received CSV from Weka: pid[~p] cat[~s] csv[~s]",
                    [From, Category, FPathCSV]),
            EmoTweets = emote_tweets(Tweets, FPathCSV),
            State#state{tweet_slots = maps:put(Category, EmoTweets, SlotMap),
                        tweet_todo  = NewTodoMap};

        error ->
            ?warning("Received unsolicited CSV: ref[~p] arg[~p]", [Ref, ArgMap]),
            State
        end,
    {noreply, NewState};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec string_to_float(Value :: string()) -> float().
%%
% @doc  Converts an integer/float in string format to a float value.
% @end  --
string_to_float(Value) ->
    case string:chr(Value, $.) of
        0 -> list_to_float(Value ++ ".0");
        _ -> list_to_float(Value)
    end.



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



%%--------------------------------------------------------------------
-spec emote_tweets(Tweets   :: tweets(),
                   FPathCSV :: string()) -> tweets().
%%
% @doc  Applies the emotion levels from the specified CSV file (filtered
%       using Weka/AfectiveTweets) to the matching list of Tweets.
%
%       NOTE: Yes, I know "emote" is intransitive.
% @end  --
emote_tweets(Tweets, FPathCSV) ->
    {ok, InCSV} = file:open(FPathCSV, [read]),
    {ok, {Cnt, EmoTweets}} = ecsv:process_csv_file_with(InCSV,
                                                        fun emote_tweets_csv/2,
                                                        {0, Tweets, []},
                                                        #ecsv_opts{quote=$'}),
    file:close(InCSV),
    ?info("Applied emotion: cnt[~B] arff[~s]", [Cnt, FPathCSV]),
    EmoTweets.



%%--------------------------------------------------------------------
-spec emote_tweets_csv(Fields  :: {newline, list()},
                       Acc     :: {integer(), tweets(), tweets()}) -> {integer(), tweets()}.
%%
% @doc  Callback function for the ecsv parser.
% @end  --
emote_tweets_csv({newline,
                  ["id",
                   "screen_name",
                   "NRC-Affect-Intensity-anger_Score",
                   "NRC-Affect-Intensity-fear_Score",
                   "NRC-Affect-Intensity-sadness_Score",
                   "NRC-Affect-Intensity-joy_Score"]},
                 Acc = {0, _, _}) ->
    %
    % This clause checks that the first line is correct
    ?debug("CSV file is correct for emote"),
    Acc;


emote_tweets_csv({eof}, {Cnt, Unprocessed, EmoTweets}) ->
    case length(Unprocessed) of
        0     -> ok;
        UnCnt -> ?warning("Unprocessed tweets after emote: cnt[~B]", [UnCnt])
    end,
    {Cnt, EmoTweets};


emote_tweets_csv({newline, [ID, ScreenName, Anger, Fear, Sadness, Joy]},
                 {Cnt, [Tweet | RestTweets], EmoTweets}) ->
    %
    ?debug("~-24s\tA:~-8s F:~-8s S:~-8s J:~-8s~n", [ScreenName, Anger, Fear, Sadness, Joy]),
    %
    % Do a sanity check on the IDs
    case (Tweet#tweet.id =:= list_to_binary(ID)) of

        true ->
            Emotions = #emotions{count   = 1,
                                 levels  = #{anger   => string_to_float(Anger),
                                             fear    => string_to_float(Fear),
                                             sadness => string_to_float(Sadness),
                                             joy     => string_to_float(Joy)}},
            EmoTweet = Tweet#tweet{emotions = Emotions},
            {Cnt + 1, RestTweets, [EmoTweet | EmoTweets]};

        false ->
            ?warning("Tweet-CSV mismatch: id[~p =/= ~p]", [Tweet#tweet.id, ID]),
            %debug("tweet id   : ~p : ~p~n", [Tweet#tweet.id, ID]),
            %debug("screen name: ~s~n", [ScreenName]),
            %debug("tweet text : ~s~n", [Tweet#tweet.text]),
            {Cnt + 1, [], EmoTweets}
    end.



%%--------------------------------------------------------------------
-spec report(Category :: atom(),
             Period   :: atom(),
             Tweets   :: tweets()) -> report().
%%
% @doc  Runs through the specified `tweet' list and prepares period data
%       for graph analysis under R.  The tuple returned to the caller
%       includes the count of tweets processed, and the generated R
%       source filename.
% @end  --
report(Category, Period, Tweets) ->
    report_aux(Tweets, Period, #report{category = Category}).



%%--------------------------------------------------------------------
-spec report_aux(Tweets :: tweets(),
                 Period :: atom(),
                 Report :: report()) -> report().
%%
% @doc  Workhorse for `report/3'.
% @end  --
report_aux([], Period, Report) ->
    ?debug("Compiled tweets for reporting: per[~s]", [Period]),
    Report;


report_aux([#tweet{timestamp_ms = Millis1970,
                   emotions     = TweetEmo} | RestTweets],
            Period,
            Report = #report{count    = Cnt,
                             beg_dts  = BegDTS,
                             end_dts  = EndDTS,
                             emotions = RptEmos}) ->
    % Tweet emotion calculations go into buckets representing the period
    DTS = dts:unix_to_datetime(Millis1970, millisecond),
    Key = case Period of
        day  -> dts:dayize(DTS);
        hour -> dts:hourize(DTS)
    end,
    %
    % NOTE: The emotion structures are evolving as we test and incorporate
    %       the various lexicons...
    NewEmo = case maps:get(Key, RptEmos, undefined) of
        undefined -> TweetEmo;
        Emo       -> emo:add(Emo, TweetEmo)
    end,
    report_aux(RestTweets, Period, Report#report{count    = Cnt + 1,
                                                 beg_dts  = dts:earlier(BegDTS, Key),
                                                 end_dts  = dts:later(EndDTS, Key),
                                                 emotions = maps:put(Key, NewEmo, RptEmos)}).

