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
%% @copyright 2017-2022 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(raven).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/1, stop/1,
         clear_cache/0,
         count_tweet_todo/1,
         emote/1,
         emote/2,
         get_big_percent/1,
         get_big_players/2,
         get_big_players/3,
         get_jvm_node/0,    get_jvm_node/1,
         is_jvm_ready/0,    is_jvm_ready/1,
         report/2,
         reset/1,
         run_tweet_csv/1]).     %% DEBUG!
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("dts.hrl").
-include("ioo.hrl").
-include("raven.hrl").
-include("types.hrl").
-include("twitter.hrl").
-include_lib("ecsv/include/ecsv.hrl").
-include_lib("llog/include/llog.hrl").

% FIXME: Do we really need separate servers for the trackers?
-define(MOD_CONFIG, #{all => #{reg => raven_cc, lot => tweet_lot_cc},
                      cc  => #{reg => raven_cc, lot => tweet_lot_cc},
                      gw  => #{reg => raven_gw, lot => tweet_lot_gw}}).
-define(mod(Key), maps:get(Key, ?MOD_CONFIG)).
-define(reg(Key), maps:get(reg, ?mod(Key), undefined)).
-define(lot(Key), maps:get(lot, ?mod(Key), undefined)).

-define(MAIN_TRACK,     gw).
-define(DEV_CACHE,      ?WORK_DIR "/dets/raven_devel").
-define(REPORT_TIMEOUT, (1 * 60 * 1000)).


% Slots are for keeping everything about a player category in one place
-record(tweet_slot, {category :: atom(),
                     players  :: players(),
                     tweets   :: tweets() }).
-type tweet_slot() :: #tweet_slot{}.


% FIXME: Merge tweet_slot functionality into tweet_lot
-record(state, {tracker           :: atom(),
                big_percent       :: float(),
                emo_report        :: rec_map(),
                tweet_slots = #{} :: map(),     % TODO: deprecated...remove soon
                tweet_todo  = #{} :: map(),     % Tweets waiting on weka processing
                jvm_node          :: atom() }).
-type state() :: #state{}.



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: atom()) -> {ok, pid()}
                                     |  ignore
                                     |  {error, term()}.
%%
% @doc  Startup function for Twitter services
% @end  --
start_link(Tracker) ->
    {ok, App} = application:get_application(),
    Args      = [Tracker,
                 application:get_env(App, big_percent, ?DEFAULT_BIG_P100),
                 application:get_env(App, jvm_node,    undefined)],

    % TODO: The raven mnesia functionality is meant to handle caching in the final design
    init_mnesia(Tracker, App),

    % The raven DETS table stores development-time shortcuts to save time on calls to Weka
    ioo:make_fpath(?DEV_CACHE),
    dets:open_file(?DEV_CACHE, [{repair, true}, {auto_save, 60000}]),

    gen_server:start_link({?REG_DIST, ?reg(Tracker)}, ?MODULE, Args, []).



%%--------------------------------------------------------------------
-spec stop(Tracker :: atom()) -> ok
                               | {error, term()}.
%%
% @doc  Shutdown function for Twitter services
% @end  --
stop(Tracker) ->
    gen_server:call(?reg(Tracker), stop),
    dets:close(?DEV_CACHE).



%%--------------------------------------------------------------------
-spec clear_cache() -> ok
                     | {error, term()}.
%%
% @doc  Clear development cache.
% @end  --
clear_cache() ->
    dets:delete_all_objects(?DEV_CACHE).



%%--------------------------------------------------------------------
-spec count_tweet_todo(Tracker :: atom()) -> non_neg_integer().
%%
% @doc  Returns the number of tweet processing requests that are
%       currently outstanding.
%%--------------------------------------------------------------------
count_tweet_todo(Tracker) ->
    gen_server:call(?reg(Tracker), count_tweet_todo).



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
%           - `stop'        End datetime to start looking for players
%           - `no_retweet'  Consider original tweets only
%           - `context'     Processing for specific context: `dic9315'
% @end  --
emote(Tracker, Option) when   is_atom(Option)
                       orelse is_tuple(Option) ->
    emote(Tracker, [Option]);


emote(Tracker, Options) ->

    % Get the full period and a version of Options without start/stop properties
    {PeriodStart,
     PeriodStop,
     RunOptions} = daily:extract_period(Tracker, Options),

    % The AUX function will make the gen_server call multiple times
    emote_aux(?reg(Tracker), PeriodStart, PeriodStop, RunOptions).



%%--------------------------------------------------------------------
-spec get_big_percent(Tracker :: atom()) -> float().
%%
% @doc  Gets the cutoff percentage for deciding who is a big player.
%%--------------------------------------------------------------------
get_big_percent(Tracker) ->
    gen_server:call(?reg(Tracker), get_big_percent).



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
-spec get_jvm_node() -> node().

-spec get_jvm_node(Tracker :: atom()) -> node().
%%
% @doc  Returns the official JVM node as specified in the application
%       configuration.  If the caller does not specify a Tracker, the
%       function returns the JVM node handling the main Tracker.
%%--------------------------------------------------------------------
get_jvm_node() ->
    get_jvm_node(?MAIN_TRACK).


get_jvm_node(Tracker) ->
    gen_server:call(?reg(Tracker), get_jvm_node).



%%--------------------------------------------------------------------
-spec is_jvm_ready() -> boolean().

-spec is_jvm_ready(Tracker :: atom()) -> boolean().
%%
% @doc  Determines if raven's JVM is available and ready.  If the
%       caller does not specify a Tracker, the function returns the
%       JVM node handling the main Tracker.
%%--------------------------------------------------------------------
is_jvm_ready() ->
    is_jvm_ready(?MAIN_TRACK).


is_jvm_ready(Tracker) ->
    case get_jvm_node(Tracker) of
        undefined -> false;
        JVM       -> ok =:= weka:ping(JVM)
    end.


%%--------------------------------------------------------------------
-spec report(Tracker :: atom(),
             Period  :: atom()) -> {ok, integer()}.
%%
% @doc  Prepares a report on tweet data (already processed by `emote')
%       for the specified time period: `hour', `day', etc.
%%--------------------------------------------------------------------
report(Tracker, Period) ->
    gen_server:call(?reg(Tracker), {report, Period}, ?REPORT_TIMEOUT).



%%--------------------------------------------------------------------
-spec reset(Tracker :: atom()) -> ok.
%%
% @doc  Reinitializes the state of the `raven' server.
%%--------------------------------------------------------------------
reset(Tracker) ->
    gen_server:call(?reg(Tracker), reset).



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
%% init:
%%
% @doc  Handles placing the first twig in Raven's data nest.
% @end  --
init([Tracker, BigP100, JVM]) ->
    ?notice("The raven is taking flight: trk[~s] big[~6.3f%] jvm[~s]", [Tracker,
                                                                        BigP100 * 100,
                                                                        JVM]),
    process_flag(trap_exit, true),

    % The big-player percentage range is: 0.0 (inclusive) to 1.0 (inclusive).
    if
        BigP100 >= 0.0 andalso
        BigP100 =< 1.0 ->
            {ok, #state{tracker     = Tracker,
                        big_percent = BigP100,
                        jvm_node    = JVM}};
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
handle_call(count_tweet_todo, _From, State) ->
    {reply, maps:size(State#state.tweet_todo), State};


handle_call({emote_day, Options}, _From, State = #state{tracker    = Tracker,
                                                        tweet_todo = Todo,
                                                        jvm_node   = JVM}) ->

    WekaCmd = proplists:get_value(context, Options, emote),     % Allow a command override
    LotDay  = proplists:get_value(start, Options),
    DayTxt  = dts:date_str(LotDay),
    FStub   = ?str_fmt("tweets.~s.~s", [Tracker, DayTxt]),
    FPath   = arff:make_fpath(tweets, FStub),

    % Pull emo-tweet from cache OR send to Weka to apply emotion filters
    NewState = case dets:lookup(?DEV_CACHE, {WekaCmd, FPath}) of

        % Cache miss! Do the work...
        [] ->
            ?debug("Pulling tweet lot from DB: day[~s]", [DayTxt]),

            % We're saving tweets here to check `id's when we get the emotion results from Weka.
            Tweets = daily_tweets:get_tweets(Tracker, Options),     % TODO: daily_tweets:spawn_link

            % Send to Weka
            ?debug("Packaging tweets for Weka: day[~s]", [DayTxt]),
            {ok, FPath} = arff:from_tweets(FStub, Tweets),

            % Send the tweet lot off to Weka on the JVM
            Lookup  = make_ref(),
            CmdArgs = jsx:encode(#{arff => FPath}),
            NewTodo = maps:put(Lookup,
                               #tweet_lot{dts    = LotDay,
                                          arff   = FPath,
                                          tweets = Tweets},
                               Todo),
            {say, JVM} ! {self(), Lookup, WekaCmd, CmdArgs},
            State#state{tweet_todo = NewTodo};

        % Cache hit! Use the Weka response from last time
        [{{WekaCmd, FPath}, Tweets, ResponseCSV}] ->
            emote_tweets(Tracker, Tweets, ResponseCSV),
            State
    end,
    {reply, ok, NewState};


handle_call(get_big_percent, _From, State) ->
    {reply, State#state.big_percent, State};


handle_call(get_jvm_node, _From, State) ->
    {reply, State#state.jvm_node, State};


handle_call(reset, _From, State) ->
    {reply, ok,
     #state{tracker     = State#state.tracker,
            big_percent = State#state.big_percent,
            jvm_node    = State#state.jvm_node},
     hibernate};


handle_call({report, Period}, _From, State = #state{emo_report  = undefined,
                                                   %tracker     = Tracker,      % R reporting
                                                   %big_percent = BigP100,      % R reporting
                                                    tweet_slots = SlotMap}) ->
    % Create two sets of three reports:
    %   (1) big-player<full, TT, RT>,
    %   (2) reg-player<full, TT, RT>
    % TODO: this intial version is coded as a one-shot, but
    %       we will need to adjust and recalcuate automatically
    %       every day/hour...
    Rpts = lists:map(fun(Size) ->
                         {Size, report(Size, Period, maps:get(Size, SlotMap, []))}
                         end,
                     [big, reg]),
    RptMap = maps:from_list(Rpts),

    % TODO: Replace with new reporting methods in next phase of project
    ?warning("R-based reporting is no longer supported."),
    %r:report_emotions(wui:get_tag(Tracker, BigP100, Period),
    %                  Period,
    %                  RptMap),
    {reply, RptMap, State#state{emo_report = RptMap}};


handle_call({report, _Period}, _From, State = #state{emo_report = RptMap}) ->
    %
    % FIXME: No check that this Period matches the last
    {reply, RptMap, State};


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
handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info({From, Ref, emote, ArgMap = #{csv := FPathCSV}}, State = #state{tracker    = Tracker,
                                                                            tweet_todo = TodoMap}) ->
    NewState = case maps:take(Ref, TodoMap) of
        {PreLot = #tweet_lot{dts    = LotDTS,
                             arff   = FPathARFF,
                             tweets = Tweets},
         NewTodoMap} ->

            ?notice("Received CSV from Weka: pid[~p] dts[~s] csv[~s]", [From,
                                                                        dts:date_str(LotDTS),
                                                                        FPathCSV]),
            EmoTweets = emote_tweets(Tracker, Tweets, FPathCSV),

            % Save lot to mnesia
            TweetLot  = PreLot#tweet_lot{tweets = EmoTweets},
            mnesia:transaction(fun()-> mnesia:write(?lot(Tracker), TweetLot, sticky_write) end),

            % For development: create a cached copy for repeated runs
            dets:insert(?DEV_CACHE, {{emote, FPathARFF}, Tweets, FPathCSV}),

            State#state{tweet_todo = NewTodoMap};

        error ->
            ?warning("Received unsolicited CSV: ref[~p] arg[~p]", [Ref, ArgMap]),
            State
        end,
    {noreply, NewState};


handle_info({From, Ref, Cmd, ArgMap = #{arff := FPathARFF,
                                        csv  := FPathCSV}}, State = #state{tweet_todo = TodoMap}) ->
    % NOTE: This clause handles special-purpose DIC functionality
    NewState = case maps:take(Ref, TodoMap) of
        {#tweet_slot{category = Category},
         NewTodoMap} ->
            ?notice("Received ARFF from Weka: pid[~p] cat[~s] arff[~s]", [From, Category, FPathARFF]),
            ?notice("Received CSV  from Weka: pid[~p] cat[~s]  csv[~s]", [From, Category, FPathCSV]),
            ?warning("No further processing: cmd[~s]", [Cmd]),
            State#state{tweet_todo  = NewTodoMap};

        error ->
            ?warning("Received unsolicited ARFF: ref[~p] arg[~p]", [Ref, ArgMap]),
            State
        end,
    {noreply, NewState};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec init_mnesia(Tracker :: atom(),
                  App     :: atom()) -> ok.
%
% @doc  Create Mnesia tables if necessary
% @end  --
init_mnesia(Tracker, App) ->
    %
    case application:get_env(App, mnesia, undefined) of
        undefined ->
            ?warning("Missing mnesia configuration");

        Mnesia ->
            % NOTE: We assume this `raven' node is a Mnesia node
            Nodes = proplists:get_value(nodes, Mnesia),
            Table = ?lot(Tracker),

            % Is the table already in msesia?
            case lists:member(Table, mnesia:system_info(tables)) of

                true ->
                    ?debug("Mnesia remembers ~s", [Table]);

                false ->
                    case mnesia:create_table(Table,
                                             [{attributes,  record_info(fields, tweet_lot)},
                                              {record_name, tweet_lot},
                                              {disc_copies, Nodes}]) of

                        {atomic,  ok}  -> ?debug("Mnesia remembering ~s", [Table]);
                        {aborted, Why} -> ?debug("Mnesia error on ~s table: why[~p]", [Table, Why])
                    end
            end
    end.



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
                          TotalCnt :: integer(),
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
                          TotalCnt   :: integer(),
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
-spec emote_aux(RavenSrv    :: atom(),
                Today       :: datetime(),
                StopDay     :: datetime(),
                Options     :: list()) -> ok.
%%
% @doc  Called from the export `emote/2' function.  Here we recurse to
%       make a gen_server call for each day in the period.
% @end  --
emote_aux(RavenSrv, Today, StopDay, Options) ->

    case daily:step(Today, StopDay, Options) of
        {Tomorrow,
         CurrOptions} ->
            gen_server:call(RavenSrv, {emote_day, CurrOptions}, ?TWITTER_DB_TIMEOUT),
            emote_aux(RavenSrv, Tomorrow, StopDay, Options);

        stop -> ok
    end.



%%--------------------------------------------------------------------
-spec emote_tweets(Tracker  :: atom(),
                   Tweets   :: tweets(),
                   FPathCSV :: string()) -> tweets().
%%
% @doc  Applies the emotion levels from the specified CSV file (filtered
%       using Weka/AfectiveTweets) to the matching list of Tweets.
%
%       NOTE: Yes, I know "emote" is intransitive.
% @end  --
emote_tweets(Tracker, Tweets, FPathCSV) ->
    {ok, InCSV} = file:open(FPathCSV, [read]),
    {ok, {Cnt, EmoTweets}} = ecsv:process_csv_file_with(InCSV,
                                                        fun emote_tweets_csv/2,
                                                        {Tracker, 0, Tweets, []},
                                                        #ecsv_opts{quote=$'}),
    file:close(InCSV),
    ?notice("Applied emotion: cnt[~5B] arff[~s]", [Cnt, FPathCSV]),
    EmoTweets.



%%--------------------------------------------------------------------
-spec emote_tweets_csv(Fields  :: {newline, list()}
                                | {eof},
                       Acc     :: {atom(), integer(), tweets(), tweets()}) -> {integer(), tweets()}.
%%
% @doc  Callback function for the ecsv parser.
% @end  --
emote_tweets_csv({newline,
                  ["id",
                   "lang",
                   "screen_name",
                   "NRC-Affect-Intensity-anger_Score",
                   "NRC-Affect-Intensity-fear_Score",
                   "NRC-Affect-Intensity-sadness_Score",
                   "NRC-Affect-Intensity-joy_Score"]},
                 Acc = {_, 0, _, _}) ->
    %
    % This clause checks that the first line is correct
    %?debug("CSV file is correct for emote"),
    Acc;


emote_tweets_csv({eof}, {Tracker, Cnt, Unprocessed, EmoTweets}) ->
    case length(Unprocessed) of
        0     -> ok;
        UnCnt -> ?warning("Unprocessed tweets after emote: trk[~s] cnt[~B]", [Tracker, UnCnt])
    end,
    {Cnt, EmoTweets};


emote_tweets_csv({newline, [ID, _Lang, ScreenName, Anger, Fear, Sadness, Joy]},
                 {Tracker, Cnt, [Tweet | RestTweets], EmoTweets}) ->
    %
    %?debug("~-24s\tA:~-8s F:~-8s S:~-8s J:~-8s~n", [ScreenName, Anger, Fear, Sadness, Joy]),
    %
    % Do a sanity check on the IDs
    case (Tweet#tweet.id =:= list_to_binary(ID)) of

        true ->
            % Add emotions, but clip text from emotionless tweets (saves memory)
            %
            % FIXME: `player' processing needs the text!
            %        Do we just forget this idea...or what?
            EmoTweet = Tweet#tweet{emotions = emo:emote(string_to_float(Anger),
                                                        string_to_float(Fear),
                                                        string_to_float(Sadness),
                                                        string_to_float(Joy))},
            %EmoTweet = emo:clip_stoic(Tweet#tweet{emotions = Emotions}),

            % Keep track of who's tweeting what!
            player:tweet(Tracker, EmoTweet),

            {Tracker, Cnt + 1, RestTweets, [EmoTweet | EmoTweets]};

        false ->
            ?error("Tweet-CSV mismatch: id[~p =/= ~p]", [Tweet#tweet.id, ID]),
            ?debug("Tweet: ~p", [Tweet]),
            ?warning("screen name: ~s~n", [ScreenName]),
            ?warning("full name  : ~s~n", [Tweet#tweet.name]),
            ?warning("description: ~s~n", [Tweet#tweet.description]),
            ?warning("tweet text : ~s~n", [Tweet#tweet.full_text]),
            exit(tweet_mismatch) %% {Cnt + 1, RestTweets, EmoTweets}
    end.



%%--------------------------------------------------------------------
-spec report(Category  :: atom(),
             Period    :: atom(),
             TweetSlot :: tweet_slot()) -> [{atom(), report()}].
%%
% @doc  Runs through `tweets' in the `tweet_slot' and prepares three
%       reports of period data for graph analysis under R:
%           - `all':     All the tweets in the `tweet_slot'
%           - `tweet':   Subset of original tweets only
%           - `retweet': Subset of retweets only
% @end  --
report(Category, Period, #tweet_slot{players = Players,
                                     tweets  = Tweets}) ->
    %
    % Create a report template as a starting base
    RptBase = #report{category    = Category,
                      player_set  = gb_sets:new()},
    %
    % And turn that into our three reports
    Reports = report_aux(Tweets, Period, [{full,    RptBase},
                                          {tweet,   RptBase},
                                          {retweet, RptBase}]),
    ?info("Report  Tweets: tt[~B] rt[~B] full[~B] tot[~B]",
          tt_rt_full(Reports, num_tweets) ++ [length(Tweets)]),

    ?info("Report Players: tt[~B] rt[~B] full[~B] tot[~B]",
          tt_rt_full(Reports, num_players) ++ [length(Players)]),
    Reports.



%%--------------------------------------------------------------------
-spec report_aux(Tweets  :: tweets(),
                 Period  :: atom(),
                 Reports :: [{atom(), report()}]) -> [{atom(), report()}].
%%
% @doc  Workhorse for `report/3'.
% @end  --
report_aux([], Period, Reports) ->
    ?debug("Compiled tweets for reporting: per[~s]", [Period]),
    Reports;


report_aux([Tweet = #tweet{timestamp_ms = Millis1970} | RestTweets],
            Period,
            Reports) ->
    % Tweet emotion calculations go into buckets representing the period
    DTS = dts:to_datetime(Millis1970, millisecond),
    Key = case Period of
        day  -> dts:dayize(DTS);
        hour -> dts:hourize(DTS)
    end,
    %
    % All reports will have the same begin/end datetimes, so just calculate from the main
    MainReport = proplists:get_value(full, Reports),
    NewBegDTS  = dts:earlier(MainReport#report.beg_dts, Key),
    NewEndDTS  = dts:later(MainReport#report.end_dts, Key),
    NewReports = lists:map(fun({Type, Report}) ->
                               {Type, report_tweet(Tweet, Key, NewBegDTS, NewEndDTS, Type, Report)}
                               end,
                           Reports),
    report_aux(RestTweets, Period, NewReports).



%%--------------------------------------------------------------------
-spec report_tweet(Tweet     :: tweet(),
                   TimeSlice :: tuple(),
                   NewBegDTS :: tuple(),
                   NewEndDTS :: tuple(),
                   RptType   :: tweet | retweet | full,
                   Report    :: report()) -> report().
%%
% @doc  Handles a single tweet for the specified report type
% @end  --
report_tweet(Tweet = #tweet{type        = TweetType,
                            screen_name = ScreenName,
                            emotions    = TweetEmo},
             TimeSlice,
             NewBegDTS,
             NewEndDTS,
             RptType,
             Report = #report{num_tweets  = TweetCnt,
                              player_set  = PlayerSet,
                              emotions    = RptEmos,
                              top_hits    = TopHits}) ->

    %?debug("Type CMP: tt[~p] rpt[~p]", [TweetType, RptType]),
    if  RptType =:= full orelse
        RptType =:= TweetType ->
            % Add in this Tweets emotion for the current day/hour
            NewEmo = case maps:get(TimeSlice, RptEmos, undefined) of
                undefined -> TweetEmo;
                Emo       -> emo:add(Emo, TweetEmo)
            end,
            NewPlayerSet = gb_sets:add_element(ScreenName, PlayerSet),
            Report#report{num_tweets  = TweetCnt + 1,
                          num_players = gb_sets:size(NewPlayerSet),
                          player_set  = NewPlayerSet,
                          beg_dts     = NewBegDTS,
                          end_dts     = NewEndDTS,
                          emotions    = maps:put(TimeSlice, NewEmo, RptEmos),
                          top_hits    = emo:do_top_hits(Tweet, TopHits)};

        TweetType =:= undefined ->
            error(bad_tweet);

        true ->
            Report
    end.




%%--------------------------------------------------------------------
-spec tt_rt_full(Reports :: reports(),
                 Field   :: atom()) -> list().
%%
% @doc  Makes a list of the values for the specfied field for the
%       `tweet', `retweet' and `all' reports.
% @end  --
tt_rt_full(Reports, Field) ->
    % NOTE: Here's a candidate for parse_trans macros
    %       https://github.com/uwiger/parse_trans
    Fields = lists:zip(record_info(fields, report),
                       lists:seq(2, record_info(size, report))),
    RecElm = proplists:get_value(Field, Fields),
    lists:map(fun(Type) ->
                  Report = proplists:get_value(Type, Reports),
                  element(RecElm, Report)
                  end,
              [tweet, retweet, full]).
