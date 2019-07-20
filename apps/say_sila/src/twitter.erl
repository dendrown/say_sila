%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Twitter access
%%
%%      Functionality for tracking and storing statuses (tweets) about
%%      ClimateChange/GlobalWarming from Twitter.
%%
%%      Note that usage of the word "hash" in this module is likely
%%      referring to Twitter hashtags, and not to hashtables or
%%      related structures.
%%
%% @copyright 2017-2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(twitter).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/0,
         stop/0,
         authenticate/1,
         %------------------------- get_* functions are getting info from the database
         get_first_dts/1,
         get_first_dts/2,
         get_players/1,
         get_players/2,
         get_tweets/2,
         get_tweets/3,
         has_hashtag/2,
         login/0,
         ontologize/1,
         ontologize/2,
         %------------------------- pull_* functions are pulling info from the Twitter API
         pull_tweet/1,  pull_tweet/2,
         reset/0,
         retrack/0,
         to_hashtag/1,
         track/0,
         track/1,
         untrack/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("dts.hrl").
-include("ioo.hrl").
-include("types.hrl").
-include("twitter.hrl").
-include_lib("llog/include/llog.hrl").

-define(STATUS_TABLE,   <<"tbl_statuses">>).
-define(STATUS_TABLES,  #{start   => <<"tbl_statuses_2017_q3_part">>,
                          q3_2017 => <<"tbl_statuses_2017_q3_part">>,
                          q4_2017 => <<"tbl_statuses_2017_q4">>,
                          q1_2018 => <<"tbl_statuses_2018_q1">>,
                          q2_2018 => <<"tbl_statuses_2018_q2">>}).
-define(status_table(X), maps:get(X, ?STATUS_TABLES, ?STATUS_TABLE)).


%%====================================================================
%% TODO:
%%  * Decide about retweets
%%  * Check tweet language
%%  * Handle extended tweets
%%====================================================================

-define(twitter_oauth_url(Cmd),  "https://api.twitter.com/oauth/"  ++ Cmd).
-define(twitter_stream_url(Cmd), "https://stream.twitter.com/1.1/" ++ Cmd).
-define(twitter_api_url(Cmd),    "https://api.twitter.com/1.1/" ++ Cmd).

% NOTE: Lookups are hashtags without the hashtag character
%       Keep lookups/hashtags defined here in all lowercase
-define(LOOK_CC, "climatechange").
-define(LOOK_GW, "globalwarming").
-define(HASH_CC, "#" ?LOOK_CC).
-define(HASH_GW, "#" ?LOOK_GW).

-define(LOOKUPS,    #{cc => ?LOOK_CC,
                      gw => ?LOOK_GW}).
-define(lookup(Key),  maps:get(Key, ?LOOKUPS, undefined)).

-define(HASHTAGS,   #{cc => ?HASH_CC,
                      gw => ?HASH_GW}).
-define(hashtag(Key), maps:get(Key, ?HASHTAGS, undefined)).

% Hard-coding a few values that should remain constant through the preliminary research
-define(DB_LANG,  <<"en">>).                        % NOTE: language affects who is a player
-define(DB_TRACK, << ?HASH_CC "," ?HASH_GW >>).


-record(state, {access       :: tuple(),
                consumer     :: tuple(),
                oauth_token  :: rec_string(),
                oauth_secret :: rec_string(),
                track        :: rec_string() | binary(),
                tracker      :: rec_pid(),
                db_conn      :: rec_pid() }).
-type state() :: #state{}.


-define(TWEET_TIMEOUT,    (5 * ?MILLIS_IN_MIN)).
-define(tweet_timeout(S), case S#state.tracker of undefined -> infinity; _ -> ?TWEET_TIMEOUT end).


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
    Args = [application:get_env(App, ?MODULE,   undefined),
            application:get_env(App, db_config, undefined)],

    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, Args, []).



%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for Twitter services
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).



%%--------------------------------------------------------------------
-spec authenticate(PIN :: string() | binary()) -> ok.
%%
% @doc  Returns the Twitter URL for the access PIN.
% @end  --
authenticate(PIN) ->
    gen_server:call(?MODULE, {authenticate, PIN}).



%%--------------------------------------------------------------------
-spec get_first_dts(Tracker :: atom()) -> integer().
%
% @doc  Returns the timestamp of the first tweet for the specified
%       `Tracker' atom: `cc' or `gw'.
% @end  --
get_first_dts(Tracker) ->
    get_first_dts(Tracker, []).



%%--------------------------------------------------------------------
-spec get_first_dts(Tracker :: atom(),
                    Options :: list()) -> integer()
                                        | tuple().
%
% @doc  Returns the timestamp of the first tweet for the specified
%       `Tracker' atom: `cc' or `gw'.
%
%       The caller may specify a list of Options.  We currently support one:
%           - `calendar' :  Gives the result as a tuple: {{year,mon,day},{hour,min,sec}}
% @end  --
get_first_dts(Tracker, Options) ->
    DTS = gen_server:call(?MODULE, {get_first_dts, Tracker}, ?TWITTER_DB_TIMEOUT),
    case proplists:get_value(calendar, Options) of
        true      -> dts:to_datetime(DTS, millisecond);
        undefined -> DTS
    end.



%%--------------------------------------------------------------------
-spec get_players(Tracker :: atom()) -> players().
%
% @doc  Returns a list of pairs of screen names and the number of
%       tweets the user has published for the specified `Tracker'
%       atom: `cc' or `gw'.
% @end  --
get_players(Tracker) ->
    get_players(Tracker, []).



%%--------------------------------------------------------------------
-spec get_players(Tracker   :: atom(),
                  Options   :: property() | proplist()) -> players().
%
% @doc  Returns a list of pairs of screen names and the number of
%       tweets the user has published for the specified `Tracker'
%       atom: `cc' or `gw'.
%
%       Options is a property list allowing the following:
%           - `min_tweets'  Only accounts having at least this many
%                           status upates (tweets) are included.
%           - `start'       Beginning datetime to start looking for players
%           - `stop'        End datetime to start looking for players
%
%       The returned list of players is sorted in order of descending
%       tweet count.
% @end  --
get_players(Tracker, Option) when   is_atom(Option)
                             orelse is_tuple(Option) ->
    get_players(Tracker, [Option]);


get_players(Tracker, Options) ->
    gen_server:call(?MODULE, {get_players, Tracker, Options}, ?TWITTER_DB_TIMEOUT).



%%--------------------------------------------------------------------
-spec get_tweets(Tracker     :: atom(),
                 ScreenNames :: binary()
                              | string()
                              | player()
                              | all) -> list().
%
% @doc  Returns the tweets for one or more accounts; `ScreenNames' can
%       take one of the following forms:
%           - <<"denDrown">>                : for one account
%           - ["denDrown", "someOneElse"]   : for multiple accounts
%           - `all'                         : all available tweets (careful!)
%
%       NOTE: this pulls only classic 140 and 280 char tweets (no
%             extended tweets)
% @end  --
get_tweets(Tracker, ScreenNames) ->
    get_tweets(Tracker, ScreenNames, []).



%%--------------------------------------------------------------------
-spec get_tweets(Tracker     :: atom(),
                 ScreenNames :: binary()
                              | string()
                              | list()
                              | player()
                              | all,
                 Options     :: list()) -> list().
%
% @doc  Returns the tweets for one or more accounts; `ScreenNames' can
%       take one of the following forms:
%           - <<"denDrown">>                : for one account
%           - ["denDrown", "someOneElse"]   : for multiple accounts
%           - `all'                         : all available tweets (careful!)
%
%       Options is a property list allowing the following:
%           - `start'       Begining datetime to start looking for tweets
%           - `stop'        End datetime to stop looking for players
%           - `no_retweet'  Collect original tweets only
%           - `mn_emo'      Call weka for an emotion analysis on the tweets
%                           and store the results in mnesia.  For this option
%                           we return the pid of the process handling the request.
%
%       NOTE: this pulls only classic 140 and 280 char tweets (no
%             extended tweets)
% @end  --
get_tweets(_, [], _) ->
    [];


get_tweets(Tracker, Player = #player{}, Options) ->
    get_tweets(Tracker, [Player], Options);


get_tweets(Tracker, ScreenName, Options) when is_binary(ScreenName) ->
    get_tweets(Tracker, binary_to_list(ScreenName), Options);


get_tweets(Tracker, ScreenNames, Options) ->
    gen_server:call(?MODULE,
                    {get_tweets, Tracker, listify_string(ScreenNames), Options},
                    ?TWITTER_DB_TIMEOUT).



%%--------------------------------------------------------------------
-spec has_hashtag(Hash :: string() | atom(),
                  Text :: string() | binary()) -> boolean().
%%
% @doc  Returns `true' if the specified tweet text contains the requested
%       hashtag; and `false' otherwise.
% @end  --
has_hashtag(Hash, Text) when is_binary(Text) ->
    has_hashtag(Hash, binary_to_list(Text));

has_hashtag([$#|Hash], Text) ->
    has_lookup(Hash, Text);

has_hashtag(Hash, Text) ->
    has_lookup(Hash, Text).



%%--------------------------------------------------------------------
-spec login() -> ok.
%%
% @doc  Logs the application into twitter.
%
%       NOTE: Currently, we just announce a PIN authentication URL and
%             prompt the sysadmin to complete the login procedure.
% @end  --
login() ->
    gen_server:cast(?MODULE, login).



%%--------------------------------------------------------------------
-spec ontologize(Tweet :: tweet()) -> [map()].
%%
% @doc  Converts the specified `Tweet' into a map with components
%       of interest for the say-sila ontology.
% @end  --
ontologize(Tweet) ->
    ontologize(Tweet, map).



%%--------------------------------------------------------------------
-spec ontologize(Tweet  :: tweet(),
                 Format :: map | json) -> [map()]
                                        | json_binary().
%%
% @doc  Converts the specified `Tweet' into a map with components
%       of interest for the say-sila ontology.
%
%       NOTE: The object property definitions are in say.sila.clj
% @end  --
ontologize(#tweet{id             = ID,
                  screen_name    = Tweeter,
                  type           = Type,
                  rt_screen_name = Author}, Format) ->
    %
    % Add `t' prefix to the tweet ID so it can be a Clojure variable
    TwID = list_to_binary(?str_fmt("t~s", [ID])),

    % The object property is an action-based role
    Action = case Type of
        tweet   -> tweets;
        retweet -> retweets
    end,

    % We always have the Tweeter tweeting|retweeting a tweet
    TweeterTweets = #{domain   => Tweeter,
                      property => Action,
                      range    => TwID},

    % Listify that, plus on a retweet,
    % also capture the Retweet retweeting the Retweeted Author (ha!)
    OntMaps = case Action of
        tweets   -> [TweeterTweets];
        retweets -> [TweeterTweets, #{domain   => TwID,
                                      property => isRetweetFrom,
                                      range    => Author}]
    end,

    % Format the ontology role mapping as requested
    case Format of
        map  -> OntMaps;
        json -> jsx:encode(OntMaps)
    end.



%%--------------------------------------------------------------------
-spec pull_tweet(ID  :: integer()
                       | string()) -> string() | map()
                                    | bad_get
                                    | bad_json.

-spec pull_tweet(ID   :: integer()
                       | string(),
                 Opts :: options()) -> string() | map()
                                     | bad_get
                                     | bad_json.
%%
% @doc  Retreives a single tweet from the Twitter API.
%
%       Supported options:
%       - return_maps   : Convert the Tweet JSON to a map
% @end  --
pull_tweet(ID) ->
    pull_tweet(ID, []).


pull_tweet(ID, Opts) ->
    gen_server:call(?MODULE, {pull_tweet, ID, Opts}).



%%--------------------------------------------------------------------
-spec reset() -> ok.
%%
% @doc  Reinitializes this server's state, and reinitializes Twitter
%       connectivity if it was previously connected.
% @end  --
reset() ->
    gen_server:cast(?MODULE, reset).



%%--------------------------------------------------------------------
-spec retrack() -> ok.
%%
% @doc  Restarts the current Twitter tracking process if one exists.
% @end  --
retrack() ->
    gen_server:cast(?MODULE, retrack).



%%--------------------------------------------------------------------
-spec to_hashtag(Tracker :: atom()) -> string()
                                     | undefined.
%%
% @doc  Returns the complete hashtag (prepended with `#') for the
%       specified `Tracker' code.
% @end  --
to_hashtag(Tracker) ->
    ?hashtag(Tracker).



%%--------------------------------------------------------------------
-spec track() -> ok.
%%
% @doc  Tracks status/tweets on Twitter for Sila's main keywords.
% @end  --
track() ->
    track(?TRACKERS).



%%--------------------------------------------------------------------
-spec track(KeyWords :: [atom()]
                      | string()
                      | binary()) -> ok.
%%
% @doc  Tracks status/tweets on Twitter for the specified `KeyWords'
% @end  --
track([Nickname | Rest]) when is_atom(Nickname)->
    KeyWords = [?hashtag(Nickname), [ io_lib:format(",~s", [?hashtag(Nick)]) || Nick <- Rest]],
    track(lists:flatten(KeyWords));


track(KeyWords) ->
    gen_server:cast(?MODULE, {track, KeyWords}).



%%--------------------------------------------------------------------
-spec untrack() -> ok.
%%
% @doc  Stops tracking Twitter status messages.
% @end  --
untrack() ->
    gen_server:cast(?MODULE, untrack).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the Twitter access server.
% @end  --
init([ArgsTw, ArgsDB]) ->
    %
    ?notice("Initializing access to Twitter: push[tbl_statuses] pull[~s]",
            [?STATUS_TABLE]),
    process_flag(trap_exit, true),

    % Make sure we've got what we need to open a connection to Twitter
    case check_twitter_config(ArgsTw) of

        {ok, [ConsKey, ConsSecret, AccessKey, AccessSecret]} ->

            ?notice("Initializing access to Twitter"),
            gen_server:cast(self(), {request_token, AccessKey, AccessSecret}),
            gen_server:cast(self(), {db_connect, ArgsDB}),

            % Are we supposed to auto-initiciate the login process??
            case proplists:get_value(login, ArgsTw) of
                true -> gen_server:cast(self(), login);
                _    -> ok
            end,

            {ok,
             #state{access   = {AccessKey, AccessSecret},
                    consumer = {ConsKey, ConsSecret, hmac_sha1}}};

        {error, Why} ->
            {stop, Why}
    end.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, #state{db_conn = DBConn}) ->
    ?notice("Twitter access shutdown: why[~p]", [Why]),
    case DBConn of
        undefined -> ok;
        _         -> epgsql:close(DBConn)
    end,
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
handle_call({authenticate, PIN}, _From, State = #state{consumer    = Consumer,
                                                       oauth_token = ReqToken}) ->

    NewState = case oauth:post(?twitter_oauth_url("access_token"),
                               [{ oauth_verifier, PIN}, { oauth_token, ReqToken}],
                               Consumer) of

        {ok, Resp = {{_, 200, _}, _, _}} ->
            Params  = oauth:params_decode(Resp),
            Token   = oauth:token(Params),
            Secret  = oauth:token_secret(Params),
            ?info("Authenticated on Twitter"),
            State#state{oauth_token  = Token, oauth_secret = Secret};

        {ok, {{_, StatCode, StatMsg}, _, ErrMsg}} ->
            ?info("Authentication failed: stat[~B:~s] msg[~s]", [StatCode, StatMsg, ErrMsg]),
            State;

        Bummer ->
            ?info("Authentication issue: ~p", [Bummer]),
            State
    end,
    {reply, ok, NewState};


handle_call({get_first_dts, Tracker}, _From, State = #state{db_conn = DBConn}) ->
    Query = io_lib:format("SELECT status->>'timestamp_ms' AS timestamp_ms "
                          "FROM ~s "
                          "WHERE track = '~s' AND hash_~s AND status->>'lang' = '~s'"
                          "ORDER BY status->'timestamp_ms' LIMIT 1",
                          [?status_table(start), ?DB_TRACK, Tracker, ?DB_LANG]),
    %?debug("QUERY: ~s", [Query]),
    Reply = case epgsql:squery(DBConn, Query) of
        {ok, _, [{DTS}]} -> binary_to_integer(DTS);
        _                -> undefined
    end,
    {reply, Reply, State};


handle_call({get_players, Tracker, Options}, _From, State = #state{db_conn = DBConn}) ->
    %
    % Prepare additions to the query according to the specified Options
    StatusTbl = get_status_table(Options),
    AndWhere  = get_timestamp_ms_sql("AND", Options),
    Having = case proplists:get_value(min_tweets, Options) of
        undefined -> "";
        MinTweets -> io_lib:format("HAVING COUNT(1) >= ~B ", [MinTweets])
    end,
    Query = io_lib:format("SELECT status->'user'->>'screen_name' AS screen_name, "
                                 "COUNT(1) AS cnt "
                          "FROM ~s "
                          "WHERE (track = '~s') AND hash_~s AND status->>'lang' = '~s' ~s "
                          "GROUP BY screen_name ~s"
                          "ORDER BY cnt DESC",
                          [StatusTbl, ?DB_TRACK, Tracker, ?DB_LANG, AndWhere, Having]),
    %?debug("QUERY: ~s", [Query]),
    Reply = case epgsql:squery(DBConn, Query) of
        {ok, _, Rows} -> [#player{screen_name = SN,
                                  tweet_cnt   = binary_to_integer(Cnt)} || {SN, Cnt} <- Rows];
        _             -> undefined
    end,
    {reply, Reply, State};


handle_call({get_tweets, Tracker, Players = [#player{} | _], Options}, From, State) ->
    %
    % Pull a list of screen names from the player recs
    ScreenNames = [Player#player.screen_name || Player <- Players],
    handle_call({get_tweets, Tracker, ScreenNames, Options}, From, State);


handle_call({get_tweets, Tracker, ScreenNames, Options}, _From, State = #state{db_conn = DBConn}) ->
    %
    % Note: This pulls only classic 140|280-char tweets and does not consider the extended_text field.
    %       Also, we're going to want to move the list_to_sql line to a DB module
    StatusTbl     = get_status_table(Options),
    TimestampCond = get_timestamp_ms_sql("AND", Options),
    %
    % Have they asked to exclude retweets?
    RetweetCond = case proplists:get_bool(no_retweet, Options) of
        true  -> <<"AND status->'retweeted_status'->>'id' is NULL">>;
        false -> <<"">>
    end,
    %
    %?debug("Screenames: ~p", [ScreenNames]),
    ScreenNamesCond = case ScreenNames =/= all of
        true  -> io_lib:format("AND status->'user'->>'screen_name' IN ('~s'~s) ",
                               [hd(ScreenNames),
                                lists:flatten([io_lib:format(",'~s'", [SN]) || SN <- tl(ScreenNames)])]);
        false -> <<"">>
    end,
    Query = io_lib:format("SELECT status->>'id' AS id, "
                                 "status->'user'->>'screen_name' AS screen_name, "
                                 "status->'user'->>'name' AS name, "
                                 "status->'user'->>'description' AS description, "
                                 "status->>'timestamp_ms' AS timestamp_ms, "
                                 "status->>'text' AS text, "
                                 "status->'retweeted_status'->>'id' AS rt_id, "
                                 "status->'retweeted_status'->'user'->>'screen_name' AS rt_screen_name "
                          "FROM ~s "
                          "WHERE track = '~s' "
                            "AND hash_~s "
                            "AND status->>'lang' ='~s' ~s ~s ~s "
                          "ORDER BY timestamp_ms",
                          [StatusTbl, ?DB_TRACK, Tracker, ?DB_LANG, TimestampCond, RetweetCond, ScreenNamesCond]),

    %file:write_file("/tmp/sila.get_tweets.sql", Query),
    Reply = case epgsql:squery(DBConn, Query) of

        {ok, _, Rows} ->
            [#tweet{id             = ID,
                    type           = ?null_val_not(RTID, tweet, retweet),
                    timestamp_ms   = binary_to_integer(DTS),
                    screen_name    = SN,
                    name           = Name,
                    description    = Descr,
                    text           = Text,
                    rt_id          = ?null_to_undef(RTID),
                    rt_screen_name = ?null_to_undef(RTSN)}
             || {ID, SN, Name, Descr, DTS, Text, RTID, RTSN} <- Rows];

        _ ->
            undefined
    end,
    {reply, Reply, State};


handle_call({pull_tweet, ID, Opts}, _From, State = #state{consumer     = Consumer,
                                                          oauth_token  = Token,
                                                          oauth_secret = Secret}) ->

    ?info("Pulling tweet #~p", [ID]),
    Reply = case oauth:get(?twitter_api_url("statuses/show.json"),
                           [{id, ID}],
                           Consumer,
                           Token,
                           Secret) of

        {ok, {{_, 200, _}, _, DataIn}} ->

            % The `jsx' library is having trouble with Twitter's JSON
            %?debug("Raw Tweet: ~p", [DataIn]),
            case pprops:get_value(return_maps, Opts, false) of
                false -> DataIn;
                true  ->
                    try jsx:decode(DataIn, [return_maps]) of
                        Tweet -> Tweet
                    catch
                        Exc:Why ->
                            ?warning("Bad JSON: why[~p:~p] data[~p]", [Exc, Why, DataIn]),
                            bad_json
                    end
            end;

        Bummer ->
            ?warning("API: ~p", [Bummer]),
            bad_get
    end,
    {reply, Reply, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State, ?tweet_timeout(State)}.



%%--------------------------------------------------------------------
%% handle_cast
%%
% @doc  Process async messages
% @end  --
handle_cast(login, State = #state{oauth_token = Token}) ->
    URL = oauth:uri(?twitter_oauth_url("authenticate"), [{oauth_token, Token}]),
    ?notice("Please retrieve your PIN from ~s~n", [URL]),
    {noreply, State};


handle_cast({request_token, AccessKey, AccessSecret}, State = #state{consumer = Consumer}) ->
    {ok, Resp} = oauth:post(?twitter_oauth_url("request_token"),
                            [{oauth_callback, oob}],
                            Consumer,
                            AccessKey,
                            AccessSecret),
    Params  = oauth:params_decode(Resp),
    Token   = oauth:token(Params),
    {noreply, State#state{oauth_token = Token}};


handle_cast({db_connect, undefined}, State) ->
    ?info("No database in use"),
    {noreply, State};


handle_cast({db_connect, DBConfig}, State) ->
    Host = proplists:get_value(host, DBConfig),
    User = proplists:get_value(user, DBConfig),
    Pass = proplists:get_value(pass, DBConfig),
    Database = proplists:get_value(database, DBConfig),
    ?info("Connecting to ~s database as ~s@~s", [Database, User, Host]),
    {ok, DBConn} = epgsql:connect(Host, User, Pass,
                                  [{database, Database},
                                   {timeout,  5000}]),
    {noreply, State#state{db_conn = DBConn}};


handle_cast(reset, State = #state{access       = Access,
                                  oauth_secret = Secret}) ->

    % Stop current tracking activity
    {_,
     MidState} = handle_cast(untrack, State),

    % Restart the OAUTH process
    gen_server:cast(self(), erlang:insert_element(1, Access, request_token)),

    % Reinitiate a login if we're connected now
    NewState = case Secret of
        undefined -> MidState;
        _         -> gen_server:cast(self(), login),
                     MidState#state{oauth_token  = undefined,
                                    oauth_secret = undefined}
    end,
    {noreply, NewState};


handle_cast(retrack, State) ->
    case State#state.tracker of
        undefined -> ok;
        Pid       -> exit(Pid, retrack)
    end,
    {noreply, State};


handle_cast({track, KeyWords}, State) ->
    TrackPid = case KeyWords of
        undefined -> undefined;
        _         -> spawn_link(fun() -> track(KeyWords, State) end)
    end,
    {noreply,
     State#state{track   = KeyWords,
                 tracker = TrackPid},
     ?tweet_timeout(State)};


handle_cast(untrack, State = #state{tracker = Pid}) ->
    case Pid of
        undefined -> ok;
        _         -> exit(Pid, untrack)
    end,
    {noreply, State#state{track = undefined}};


handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State, ?tweet_timeout(State)}.


%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info(timeout, State) ->
    %
    case State#state.tracker of
        undefined -> ok;
        Pid ->
            ?warning("Twitter feed stalled on ~p", [Pid]),
            gen_server:cast(self(), retrack)
    end,
    {noreply, State};


handle_info({track_headers, Headers}, State) ->
    lists:foreach(fun({Hdr, Val}) ->
                      ?info("Track header ~s: ~s", [Hdr, Val]) end,
                  Headers),
    {noreply, State, ?tweet_timeout(State)};


handle_info({track, <<>>}, State) ->
    ?debug("Silent tweet"),
    {noreply, State, ?tweet_timeout(State)};


handle_info({track, DataIn}, State) ->

    %?info("Tracking in: ~p", [DataIn]),
    try jsx:decode(DataIn, [return_maps]) of
        Tweet ->
            %?debug("Raw Tweet: ~p", [Tweet])
            log_tweet(Tweet),

            % A real tweet will have a text field
            case maps:get(<<"text">>, Tweet, undefined) of
                undefined ->
                    ok;
                Text ->
                    store_tweet(DataIn, has_lookup(cc, Text), has_lookup(gw, Text), State)
            end
    catch
        Exc:Why -> ?warning("Bad JSON: why[~p:~p] data[~p]", [Exc, Why, DataIn])
    end,
    ?info("END OF TWEET"),
    {noreply, State, ?tweet_timeout(State)};


handle_info({'EXIT', Pid, Why}, State = #state{tracker = Tracker}) ->

    NewState = case Pid of
        Tracker ->
            % Check if we need to restart the tracker process
            ?info("Stopped tracker on ~p: why[~p]", [Pid, Why]),
            case Why of
                retrack -> gen_server:cast(self(), {track, State#state.track});
                _       -> ok
            end,
            State#state{tracker = undefined};
        _ ->
            ?warning("Unknown tracker exited: pid[~p]", [Pid]),
            State
    end,
    {noreply, NewState};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State, ?tweet_timeout(State)}.


%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec check_twitter_config(TwappCfg :: list()) -> {ok, list()}
                                                | {error, string()}.
%%
% @doc  Verifies that our configuration has all the twitter it needs.
%       If so, we return just the parameter values in a list.
%
%       The idea is to not just "let it fail" in the middle of a Twitter
%       handshake, as we might get cut off after so many failed attempts.
% @end  --
check_twitter_config(TwappCfg) ->
    case lists:map(fun(Param) -> proplists:get_value(Param, TwappCfg, nak) end,
                   [consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret]) of

        [nak, _, _, _]  -> {error, "Missing Twitter consumer key"};
        [_, nak, _, _]  -> {error, "Missing Twitter consumer secret"};
        [_, _, nak, _]  -> {error, "Missing Twitter access key"};
        [_, _, _, nak]  -> {error, "Missing Twitter access secret"};

        Params          -> {ok, Params}
    end.



%%--------------------------------------------------------------------
-spec listify_string(S :: string()
                        | term()) -> list() | term().
%%
% @doc  Turn a single string or printable binary into a one-item list
%       of that item.  If the input is already a normal list (or really,
%       anything other than a singleton string), the we return it as is.
% @end  --
listify_string(S) ->
    case io_lib:printable_unicode_list(S) of
        true  -> [S];                         % ["justOne"]
        false -> S
    end.



%%--------------------------------------------------------------------
-spec track(KeyWords :: binary()
                      | string(),
            State    :: term()) -> ok
                                 | bad_post.
%%
% @doc  Stream a tracking command
% @end  --
track(KeyWords, #state{consumer     = Consumer,
                       oauth_token  = Token,
                       oauth_secret = Secret}) ->
    case oauth:post(?twitter_stream_url("statuses/filter.json"),
                    [{track, KeyWords}],
                    Consumer,
                    Token,
                    Secret,
                    [{sync, false}, {stream, self}]) of

        {ok, ReqID} ->
            ?debug("Tracking on ~p: ~s", [self(), KeyWords]),
            stream_track(ReqID);

        {error, Why} ->
            ?debug("Tracking failure on ~p: ~s", [self(), Why]),
            bad_post
    end.


%%--------------------------------------------------------------------
-spec stream_track(ReqID  :: term()) -> ok.
%%
% @doc  Recursively stream tracking input.
% @end  --
stream_track(ReqID) ->
    stream_track(ReqID, <<>>).



%%--------------------------------------------------------------------
-spec stream_track(ReqID  :: term(),
                   Prefix :: binary()) -> ok.
%%
% @doc  Recursively stream tracking input.  `Prefix' is data we've
%       read in already, but which does not constitute a complete packet.
% @end  --
stream_track(ReqID, Prefix) ->
    receive
        {http, {ReqID, stream_start, Hdrs}} ->
            ?MODULE ! {track_headers, Hdrs},
            stream_track(ReqID, Prefix);

        {http, {ReqID, stream, DataIn}} ->
            % Glue the old and new data, then split it again on the packet delimiter
            DataMerge = iolist_to_binary([Prefix, DataIn]),
            DataSplit = binary:split(DataMerge, <<"\r\n">>, [global]),
            NewPrefix = process_track(DataSplit),
            stream_track(ReqID, NewPrefix);

        {http, {ReqID, {error, Why}}} ->
            ?info("Error tracking on ~p: why[~p]", [self(), Why]);

        {_, stop} ->
            ?info("Stopped tracking on ~p", [self()]);

        _ ->
            stream_track(ReqID, Prefix)
    end.


%%--------------------------------------------------------------------
-spec process_track(Data :: [binary()]) -> binary().
%%
% @doc  Split track data into JSON packets and forward to our Twitter
%       server for processing.  Note that `Data' containing at least
%       one full packet will have at least two list items, and if it
%       represents a single packet, the `Extra' data will be empty.
% @end  --
process_track([Extra]) ->
    Extra;


process_track([Packet, Extra]) ->
    ?MODULE ! {track, Packet},
    Extra;


process_track([Packet | Rest]) ->
    ?MODULE ! {track, Packet},
    process_track(Rest).


%%--------------------------------------------------------------------
-spec log_tweet(Tweet :: map()) -> ok.
%%
% @doc  Pretty-prints the information contained in a tweet over
%       multiple log lines.
% @end  --
log_tweet(Tweet) ->
    log_tweet("", Tweet).



%%--------------------------------------------------------------------
-spec log_tweet(Indent :: string(),
                MapKV  :: map()
                        | {binary(), term()}) -> ok.
%%
% @doc  Recursively logs the information contained in a tweet
% @end  --
log_tweet(_, {_, null}) ->
    ok;


log_tweet(_, {_, []}) ->
    ok;


log_tweet(Indent, Map) when is_map(Map) ->
    log_subtweet(Indent, maps:to_list(Map));


log_tweet(Indent, {Key, Val}) when is_map(Val) ->
    log_subtweet(Indent, Key, maps:to_list(Val));


log_tweet(Indent, {Key, MapList = [MapVal | _]}) when is_map(MapVal) ->
    log_subtweet(Indent, Key, MapList);


log_tweet(Indent, {Key, Val}) ->

    % Highlight the actual tweet text
    Level = case Key of
        <<"text">> -> notice;
        _          -> debug
    end,

    % Tame any UTF8 because lager doesn't handle it well
    {Format, SaneValue} = if
        is_binary(Val) -> {"~s~s: ~s", re:replace(Val, "[^\ -~]", ".", [global])};
        true           -> {"~s~s: ~p", Val}
    end,

    ?llog(Level, Format, [Indent, Key, SaneValue]).



%%--------------------------------------------------------------------
-spec log_subtweet(Indent   :: string(),
                   Key      :: binary(),
                   SubTweet :: map() | list()) -> ok.
%%
% @doc  Logs the information contained in a tweet substructure
% @end  --
log_subtweet(Indent, Key,  SubTweet) ->
    ?debug("~s~s:", [Indent, Key]),
    log_subtweet(Indent, SubTweet).



%%--------------------------------------------------------------------
-spec log_subtweet(Indent   :: string(),
                   SubTweet :: list()) -> ok.
%%
% @doc  Logs the information contained in a tweet substructure
% @end  --
log_subtweet(Indent, SubTweet) ->
    NewIndent = "  " ++ Indent,
    lists:foreach(fun(Elem) -> log_tweet(NewIndent, Elem) end, SubTweet).



%%--------------------------------------------------------------------
-spec store_tweet(RawTweet  :: binary(),
                  IsCC      :: boolean(),
                  IsGW      :: boolean(),
                  State     :: state()) -> ok.
%%
% @doc  Inserts the tweet into the database, if we're connected.
% @end  --
store_tweet(_, _, _, #state{db_conn = undefined}) ->
    ok;

store_tweet(RawTweet, IsCC, IsGW, #state{db_conn = DBConn,
                                         track   = Track}) ->
    %
    % IMPORTANT: Note that we pull results from the twitter<gen_server> state variable,
    %            but for now we always write new tweets to the main statuses table.
    DBResult = epgsql:equery(DBConn,
                             "INSERT INTO tbl_statuses (status, track, hash_cc, hash_gw) "
                             "VALUES  ($1, $2, $3, $4)",
                             [RawTweet, Track, IsCC, IsGW]),
    ?info("Tweet stored: ~p", [DBResult]).




%%--------------------------------------------------------------------
-spec has_lookup(Look :: string() | atom(),
                 Text :: string() | binary()) -> boolean().
%%
% @doc  Returns `true' if the specified tweet text contains the requested
%       lookup keyword (hashtag without the hash); and `false' otherwise.
%
%       NOTE: The `string' library is enhanced in Erlang/OTP 20, and this
%             function may benefit from some rework when we move onto it.
% @end  --
has_lookup(Look, Text) when is_atom(Look) ->
    has_lookup(?lookup(Look), Text);

has_lookup(Look, Text) when is_binary(Text) ->
    has_lookup(Look, binary_to_list(Text));

has_lookup(Look, Text) ->
    TextParts = string:tokens(string:to_lower(Text), "#"),

    % The first part of the tweet will be the same whether or not it had a
    % hashtag, so throw it away if it didn't
    Lookups  = case ($# =:= hd(Text)) of
        true  -> TextParts;
        false -> tl(TextParts)
    end,
    is_prefix_in_parts(Look, Lookups).


%%--------------------------------------------------------------------
-spec is_prefix_in_parts(Prefix    :: string(),
                         TextParts :: [string()]) -> boolean().
%%
% @doc  Looks for the `Prefix' as the first word in any of the `TextParts'.
% @end  --
is_prefix_in_parts(_, []) ->
    false;

is_prefix_in_parts(Lookup, [Part|Rest]) ->
    %?debug("Checking for '~s' in '~s'", [Lookup, Part]),
    case string:tokens(Part, " ") of
        [Word|_] when Word =:= Lookup -> true;
        _                             -> is_prefix_in_parts(Lookup, Rest)
    end.



%%--------------------------------------------------------------------
-spec get_status_table(Options :: atom()
                                | list()) -> binary().
%%
% @doc  Returns the table holding the tweet data between the `start'
%       and `stop' elements from `Options'.  Currently, we require that
%       the time period be covered by a single status table.
% @end  --
get_status_table(Key) when is_atom(Key) ->
   ?status_table(Key);


get_status_table(Options) ->
    %
    % Get start/stop date-times
    Tabler = fun(Key) ->
                 case proplists:lookup(Key, Options) of
                     none         -> undefined;
                     {start, DTS} -> ?status_table(dts:quarter(DTS));
                     {stop,  DTS} -> ?status_table(dts:quarter(dts:sub(DTS, 1, second)))
                 end end,
    Tables = lists:map(Tabler, [start, stop]),

    % Make sure we didn't get two different tables
    % TODO: We'll probably want to handle this eventually
    case lists:filter(fun(P) -> P =/= undefined end, Tables) of
        []          -> ?STATUS_TABLE;
        [Tbl]       -> Tbl;
        [Tbl, Tbl]  -> Tbl
    end.



%%--------------------------------------------------------------------
-spec get_timestamp_ms_sql(Prefix   :: string()
                                     | binary(),
                           Options  :: list()) -> string().
%%
% @doc  Returns a timestamp constraint for an SQL query if the Options
%       proplist calls for one with a `start' or `stop' element.  Allows
%       for a Prefix for the expression, usually "AND", "WHERE" or an
%       empty string.
% @end  --
get_timestamp_ms_sql(Prefix, Options) ->
    %
    % Get start/stop date-times
    GetPeriod = fun({undefined, _})    -> undefined;
                   ({DateTime, CmpOp}) -> io_lib:format("(status->>'timestamp_ms' ~s '~B')",
                                                        [CmpOp,
                                                         dts:to_unix(DateTime, millisecond)])
                                          end,
    Period = lists:map(GetPeriod, [{proplists:get_value(start, Options), <<">=">>},     % Inclusive
                                   {proplists:get_value(stop,  Options), <<"<">>}]),    % Exclusive

    % Build the SQL WHERE clause component
    case lists:filter(fun(P) -> P =/= undefined end, Period) of
        [Cut]       -> io_lib:format("~s ~s", [Prefix, Cut]);
        [Beg, End]  -> io_lib:format("~s (~s AND ~s)", [Prefix, Beg, End])
    end.
