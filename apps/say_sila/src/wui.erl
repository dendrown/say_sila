%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Web User Interface (WUI) server for Say-Sila
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(wui).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/1,
        stop/0,
        configure/0,
        get_conf/0,
        get_graph_dir/0,
        get_status_dir/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("wui.hrl").


% TODO: Move config to the configuration
-define(ID,         "wui").
-define(DOC_ROOT,   ?WORK_DIR "/www").
-define(LOG_DIR,    ?WORK_DIR "/log").
-define(GRAPH_DIR,  ?DOC_ROOT "/graph").
-define(STATUS_DIR, ?DOC_ROOT "/status").
-define(GCONFS,     [{id, ?ID},
                     {logdir,     ?LOG_DIR}]).
-define(SCONFS,     [{port,       8080},
                     {servername, "sila"},
                     {listen,     {0,0,0,0}},
                     {docroot,    ?DOC_ROOT},
                     {appmods,    [{"/emote", wui_emote}]} ]).


-record(state, {yaws :: yaws_conf()}).
%type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(yaws_conf()) -> {ok, pid()}
                              |  ignore
                              |  {error, term()}.
%%
% @doc  Startup function for the web interface module.
% @end  --
start_link(Conf) ->
    ok = filelib:ensure_dir(?LOG_DIR "/.keep"),
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, [Conf], []).


%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for the web interface
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).


%%--------------------------------------------------------------------
-spec configure() -> any().
%%
% @doc  Sets the configuration for the YAWS service.  Be sure the
%       supervisor has started the child processes before calling
%       this function.
% @end  --
configure() ->
    gen_server:call(?MODULE, configure).



%%--------------------------------------------------------------------
-spec get_conf() -> [tuple()].
%%
% @doc  Gets child process run specifications for YAWS processes.
%       A supervisor should call this function.
% @end  --
get_conf() ->
    {ok, SConfs, GConf, ChildSpecs} = yaws_api:embedded_start_conf(?DOC_ROOT,
                                                                   ?SCONFS,
                                                                   ?GCONFS,
                                                                   ?ID),
    Conf = #yaws_conf{id     = ?ID,
                      gConf  = GConf,
                      sConfs = SConfs,
                      childSpecs = ChildSpecs},

    ?debug("YAWS: id[~p]",   [Conf#yaws_conf.id]),
    ?debug("YAWS: glob[~p]", [Conf#yaws_conf.gConf]),
    ?debug("YAWS: srvs[~p]", [Conf#yaws_conf.sConfs]),
    ?debug("YAWS: chSp[~p]", [Conf#yaws_conf.childSpecs]),
    Conf.



%%--------------------------------------------------------------------
-spec get_graph_dir() -> string().
%%
% @doc  Reports the directory where the WUI expects charts and graphs.
% @end  --
get_graph_dir() ->
    ?GRAPH_DIR.



%%--------------------------------------------------------------------
-spec get_status_dir() -> string().
%%
% @doc  Reports the directory where the WUI expects status and
%       statistical information.
% @end  --
get_status_dir() ->
    ?STATUS_DIR.



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the cortex server.
% @end  --
init([Conf]) ->
    ?notice("Web User Interface ON: id[~s]", [Conf#yaws_conf.id]),
    process_flag(trap_exit, true),

    {ok, #state{yaws = Conf}}.


%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: term()) -> ok.
%%
% @doc  Server shutdown callback.  Shuts down the AMQP queue and associated
%       channel and connection.
% @end  --
terminate(Why, _State) ->
    ?notice("Web User Interface OFF: why[~p]", [Why]),
    ok.


%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(),
                  State  :: term(),
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
handle_call(configure, _From, State) ->
    Conf = State#state.yaws,
    ?info("Setting YAWS configuration ~s", [Conf#yaws_conf.id]),
    ConfStatus = yaws_api:setconf(Conf#yaws_conf.gConf, Conf#yaws_conf.sConfs),
    {reply, ConfStatus, State};


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
handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================
