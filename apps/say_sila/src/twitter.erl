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
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(twitter).
-behaviour(gen_server).


-export([start_link/0,
         stop/0,
         get_pin/0,
         authenticate/1,
         track/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").

-define(twitter_oauth_url(Cmd),  "https://api.twitter.com/oauth/"  ++ Cmd).
-define(twitter_stream_url(Cmd), "https://stream.twitter.com/1.1/" ++ Cmd).


-record(state, {consumer     :: tuple(),
                oauth_token  :: string(),
                oauth_secret :: string() }).
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

        Args ->
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
-spec get_pin() -> string()
                 | undefined.
%%
% @doc  Returns the Twitter URL for the access PIN.
% @end  --
get_pin() ->
    gen_server:call(?MODULE, get_pin).



%%--------------------------------------------------------------------
-spec authenticate(PIN :: string() | binary()) -> ok.
%%
% @doc  Returns the Twitter URL for the access PIN.
% @end  --
authenticate(PIN) ->
    gen_server:call(?MODULE, {authenticate, PIN}).


%%--------------------------------------------------------------------
-spec track(KeyWords :: string() | binary()) -> ok.
%%
% @doc  Tracks status/tweets on Twitter for the specified `KeyWords'
% @end  --
track(KeyWords) ->
    gen_server:cast(?MODULE, {track, KeyWords}).




%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(list()) -> any().
%%
% @doc  Initialization for the Twitter access server.
% @end  --
init([ConsKey, ConsSecret, AccessKey, AccessSecret]) ->
    ?notice("Initializing access to Twitter"),
    process_flag(trap_exit, true),

    gen_server:cast(self(), {request_token, AccessKey, AccessSecret}),

    {ok, #state{consumer = {ConsKey, ConsSecret, hmac_sha1}}}.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _State) ->
    ?notice("Twitter access shutdown: why[~p]", [Why]),
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
handle_cast({request_token, AccessKey, AccessSecret}, State = #state{consumer = Consumer}) ->
    {ok, Resp} = oauth:post(?twitter_oauth_url("request_token"),
                            [{oauth_callback, oob}],
                            Consumer,
                            AccessKey,
                            AccessSecret),
    Params  = oauth:params_decode(Resp),
    Token   = oauth:token(Params),
    {noreply, State#state{oauth_token = Token}};


handle_cast({track, KeyWords}, State) ->
    PID = spawn_link(fun() -> track(KeyWords, State) end),
    ?debug("Tracking on ~p: ~s", [PID, KeyWords]),
    {noreply, State};


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
            ?notice("END OF TWEET~n")
    catch
        Exc:Why -> ?warning("Bad JSON: why[~p:~p]", [Exc, Why])
    end,
    {noreply, State};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
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
%       represents a single packet, the `Extra' data will be <<>>.
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
-spec log_tweet(map()) -> ok.
%%
% @doc  Pretty-prints the information contained in a tweet over
%       multiple log lines.
% @end  --
log_tweet(Tweet) ->
    log_tweet("", Tweet).



%%--------------------------------------------------------------------
-spec log_tweet(Indent :: string(),
                Pair   :: {binary(), term()}) -> ok.
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
log_subtweet(Indent, SubTweet)  ->
    NewIndent = "  " ++ Indent,
    lists:foreach(fun(Elem) -> log_tweet(NewIndent, Elem) end, SubTweet).

