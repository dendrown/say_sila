%%%-------------------------------------------------------------------
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
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(twitter).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/0,
         stop/0,
         authenticate/1,
         get_pin/0,
         login/0,
         track/1,
         get_first_dts/1,
         get_first_dts/2,
         get_players/1,
         get_players/2,
         get_tweets/2,
         get_tweets/3,
         has_hashtag/2,
         to_hashtag/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("types.hrl").
-include("twitter.hrl").
-include_lib("llog/include/llog.hrl").



%%====================================================================
%% TODO:
%%  * Decide about retweets
%%  * Check tweet language
%%  * Handle extended tweets
%%====================================================================

-define(twitter_oauth_url(Cmd),  "https://api.twitter.com/oauth/"  ++ Cmd).
-define(twitter_stream_url(Cmd), "https://stream.twitter.com/1.1/" ++ Cmd).

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


-record(state, {consumer     :: tuple(),
                oauth_token  :: rec_string(),
                oauth_secret :: rec_string(),
                track        :: rec_string() | binary(),
                db_conn      :: rec_pid(),
                status_tbl   :: rec_string() }).
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
    case lists:map(fun(Param) -> application:get_env(App, Param, nak) end,
                   [twitter_consumer_key,
                    twitter_consumer_secret,
                    twitter_access_token,
                    twitter_access_secret]) of

        [nak, _, _, _]  -> {error, "Missing Twitter consumer key"};
        [_, nak, _, _]  -> {error, "Missing Twitter consumer secret"};
        [_, _, nak, _]  -> {error, "Missing Twitter access key"};
        [_, _, _, nak]  -> {error, "Missing Twitter access secret"};

        Twitter ->
            % TODO: The DB functionality will be moving to its own module
            Args = [application:get_env(App, db_config, undefined) | Twitter],
            gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, Args, [])
    end.



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
-spec get_pin() -> string()
                 | undefined.
%%
% @doc  Returns the Twitter URL for the access PIN.
% @end  --
get_pin() ->
    gen_server:call(?MODULE, get_pin).



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
-spec track(KeyWords :: string() | binary()) -> ok.
%%
% @doc  Tracks status/tweets on Twitter for the specified `KeyWords'
% @end  --
track([Nickname | Rest]) when is_atom(Nickname)->
    KeyWords = [?hashtag(Nickname), [ io_lib:format(",~s", [?hashtag(Nick)]) || Nick <- Rest]],
    track(lists:flatten(KeyWords));


track(KeyWords) ->
    gen_server:cast(?MODULE, {track, KeyWords}).



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
    DTS = gen_server:call(?MODULE, {get_first_dts, Tracker}),
    case proplists:get_value(calendar, Options) of
        true      -> dts:unix_to_datetime(DTS, millisecond);
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
%           - `stop'        End datetime to start looking for players
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
    gen_server:call(?MODULE, {get_tweets, Tracker, listify_string(ScreenNames), Options}, ?TWITTER_DB_TIMEOUT).



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
-spec to_hashtag(Tracker :: atom()) -> string()
                                     | undefined.
%%
% @doc  Returns the complete hashtag (prepended with `#') for the
%       specified `Tracker' code.
% @end  --
to_hashtag(Tracker) ->
    ?hashtag(Tracker).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the Twitter access server.
% @end  --
init([DBConfig, ConsKey, ConsSecret, AccessKey, AccessSecret]) ->
    %
    % IMPORTANT: Note that we pull results from the twitter<gen_server> state variable,
    %            but for now we always write new tweets to the main statuses table.
    StatusTbl = proplists:get_value(status_tbl, DBConfig, <<"tbl_statuses">>),
    ?notice("Initializing access to Twitter: push[tbl_statuses] pull[~s]",
            [StatusTbl]),
    process_flag(trap_exit, true),

    gen_server:cast(self(), {request_token, AccessKey, AccessSecret}),
    gen_server:cast(self(), {db_connect, DBConfig}),

    {ok, #state{consumer   = {ConsKey, ConsSecret, hmac_sha1},
                status_tbl = StatusTbl}}.



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
handle_call(get_pin, _From, State = #state{oauth_token = Token}) ->
    URL = oauth:uri(?twitter_oauth_url("authenticate"), [{oauth_token, Token}]),
    {reply, URL, State};


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


handle_call({get_first_dts, Tracker}, _From, State = #state{db_conn    = DBConn,
                                                            status_tbl = StatusTbl}) ->
    Query = io_lib:format("SELECT status->>'timestamp_ms' AS timestamp_ms "
                          "FROM ~s "
                          "WHERE track = '~s' AND hash_~s AND status->>'lang' = '~s'"
                          "ORDER BY status->'timestamp_ms' LIMIT 1",
                          [StatusTbl, ?DB_TRACK, Tracker, ?DB_LANG]),
    %?debug("QUERY: ~s", [Query]),
    Reply = case epgsql:squery(DBConn, Query) of
        {ok, _, [{DTS}]} -> binary_to_integer(DTS);
        _                -> undefined
    end,
    {reply, Reply, State};


handle_call({get_players, Tracker, Options}, _From, State = #state{db_conn    = DBConn,
                                                                   status_tbl = StatusTbl}) ->
    %
    % Prepare additions to the query according to the specified Options
    AndWhere = get_timestamp_ms_sql("AND", Options),
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


handle_call({get_tweets, Tracker, ScreenNames, Options}, _From, State = #state{db_conn    = DBConn,
                                                                               status_tbl = StatusTbl}) ->
    %
    % Note: This pulls only classic 140|280-char tweets and does not consider the extended_text field.
    %       Also, we're going to want to move the list_to_sql line to a DB module
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
        {ok, _, Rows} -> [#tweet{id             = ID,
                                 screen_name    = SN,
                                 timestamp_ms   = binary_to_integer(DTS),
                                 text           = Text,
                                 type           = ?null_val_not(RTID, tweet, retweet),
                                 rt_id          = ?null_to_undef(RTID),
                                 rt_screen_name = ?null_to_undef(RTSN)} || {ID, SN, DTS, Text, RTID, RTSN} <- Rows];
        _             -> undefined
    end,
    {reply, Reply, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_cast
%%
% @doc  Process async messages
% @end  --
handle_cast(login, State) ->
    URL = get_pin(),
    ?notice("Please retrieve your PIN from ~s~n", [URL]),

    % TODO: We need a proper UI for PIN entry
    % {ok, PIN} = io:fread("PIN> ", "~s"),
    % authenticate(PIN),
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


handle_cast({track, KeyWords}, State) ->
    PID = spawn_link(fun() -> track(KeyWords, State) end),
    ?debug("Tracking on ~p: ~s", [PID, KeyWords]),
    {noreply, State#state{track = KeyWords}};


handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info({track_headers, Headers}, State) ->
    lists:foreach(fun({Hdr, Val}) ->
                      ?info("Track header ~s: ~s", [Hdr, Val]) end,
                  Headers),
    {noreply, State};


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
    ?notice("END OF TWEET"),
    {noreply, State};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
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
    Fmt = if
        is_binary(Val) orelse
        is_atom(Val)        -> "~s~s: ~s";
        is_integer(Val)     -> "~s~s: ~B";
        is_list(Val)        -> "~s~s: ~p"
    end,
    case Key of
        <<"text">>  -> ?info(Fmt, [Indent, Key, Val]);      % Tweet text
        _           -> ?debug(Fmt, [Indent, Key, Val])
    end.



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
                                                         dts:datetime_to_unix(DateTime, millisecond)])
                                          end,
    Period = lists:map(GetPeriod, [{proplists:get_value(start, Options), <<">=">>},     % Inclusive
                                   {proplists:get_value(stop,  Options), <<"<">>}]),    % Exclusive

    % Build the SQL WHERE clause component
    case lists:filter(fun(P) -> P =/= undefined end, Period) of
        [Cut]       -> io_lib:format("~s ~s", [Prefix, Cut]);
        [Beg, End]  -> io_lib:format("~s (~s AND ~s)", [Prefix, Beg, End])
    end.
