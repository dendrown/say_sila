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
         authenticate/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").

-define(twitter_oauth_url(Cmd), "https://api.twitter.com/oauth/" ++ Cmd).

-record(state, {consumer     :: tuple(),
                oauth_token  :: string(),
                oauth_secret :: string() }).
-type state() :: #state{}.


%%% TODO: API Example:
%%%
%%% {ok, {_,_,IPCC}} = oauth:get("https://api.twitter.com/1.1/users/show.json",
%%%                              [{screen_name, <<"IPCC_CH">>}],
%%%                              {ConsumerKey, ConsumerSecret, hmac_sha1},
%%%                              #state.oauth_token,
%%%                              #state.oauth_secret).
%%%


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
-spec handle_call(Msg   :: term(),
                  From  :: {pid(), term()},
                  State :: state()) -> any().
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call(get_pin, _From, State = #state{oauth_token = Token}) ->
    URL = oauth:uri(?twitter_oauth_url("authenticate"), [{oauth_token, Token}]),
    {reply, URL, State};


handle_call({authenticate, PIN}, _From, State = #state{consumer    = Consumer,
                                                       oauth_token = ReqToken}) ->
    ?debug("Authenticating<~s>", [PIN]),
    {ok, Resp} = oauth:post(?twitter_oauth_url("access_token"),
                            [{ oauth_verifier, PIN}, { oauth_token, ReqToken}],
                            Consumer),
    Params  = oauth:params_decode(Resp),
    Token   = oauth:token(Params),
    Secret  = oauth:token_secret(Params),
    {reply, ok, State#state{oauth_token  = Token,
                            oauth_secret = Secret}};


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
handle_cast({request_token, AccessKey, AccessSecret}, State = #state{consumer = Consumer}) ->
    {ok, Resp} = oauth:post(?twitter_oauth_url("request_token"),
                            [{oauth_callback, oob}],
                            Consumer,
                            AccessKey,
                            AccessSecret),
    Params  = oauth:params_decode(Resp),
    Token   = oauth:token(Params),
    {noreply, State#state{oauth_token = Token}};


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
%%====================================================================
