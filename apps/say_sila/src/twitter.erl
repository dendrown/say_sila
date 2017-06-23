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
    PID = spawn_link(fun() -> stream_track(KeyWords, State) end),
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
handle_info({track, DataIn}, State) ->

    ?info("Tracking in: ~p", [DataIn]),
   %try jsx:decode(DataIn, [return_maps]) of
   %    Tweet -> ?info("Tweet: ~p", [Tweet])
   %catch
   %    Exc:Why -> ?warning("Bad JSON: why[~p:~p]", [Exc, Why])
   %end,
    {noreply, State};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec stream_track(KeyWords :: binary()
                             | string(),
                   State    :: term()) -> ok
                                        | bad_post.
%%
% @doc  Stream a tracking command
% @end  --
stream_track(KeyWords, #state{consumer     = Consumer,
                              oauth_token  = Token,
                              oauth_secret = Secret}) ->
    case oauth:post(?twitter_stream_url("statuses/filter.json"),
                    [{delimited, <<"length">>},
                     {track,     <<"climate">>}],
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
-spec stream_track(ReqID :: term()) -> ok.
%%
% @doc  Recursively stream tracking input
% @end  --
stream_track(ReqID) ->
    receive
        {http, {ReqID, stream_start, Hdrs}} ->
          ?MODULE ! {self(), {stream_start, Hdrs}},
          stream_track(ReqID);

        {http, {ReqID, stream, DataIn}} ->
          case DataIn of
            <<"\r\n">> -> ok;
            _          -> ?MODULE ! {track, DataIn}
          end,
          stream_track(ReqID);

        {http, {ReqID, {error, Why}}} ->
            ?info("Error tracking on ~p: why[~p]", [self(), Why]);

        {_, stop} ->
            ?info("Stopped tracking on ~p", [self()]);

        _ ->
          stream_track(ReqID)
    end.
