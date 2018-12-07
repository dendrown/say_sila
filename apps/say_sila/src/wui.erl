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
        get_comms/1,        get_comms/2,
        get_comms_atom/1,   get_comms_atom/2,
        get_conf/0,
        get_graph_dir/0,
        get_status_dir/0,
        get_reports/1,
        get_tag/1,          get_tag/3,
        get_track/1,        get_track/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("raven.hrl").
-include("wui.hrl").
-include_lib("llog/include/llog.hrl").


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

-define(GOOD_TRACKS, ["cc", "gw"]).     % TODO: Link to `twitter' definitions


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
-spec get_comms(Arg :: arg()) -> binary() | undefined.
%%
% @doc  Returns the communications code requested in the YAWS `querydata';
%       or `undefined' if the comms code is not available.
% @end  --
get_comms(Arg) ->
    get_comms(Arg, undefined).



%%--------------------------------------------------------------------
-spec get_comms(Arg     :: arg(),
                Default :: term()) -> term().
%%
% @doc  Returns the communications code requested in the YAWS `querydata';
%       or the specified Default if the comms code is not available.
% @end  --
get_comms(Arg, Default) ->
    case yaws_api:queryvar(Arg, "comms") of
        {ok, "full"}    -> <<"full">>;
        {ok, "tweet"}   -> <<"tweet">>;
        {ok, "retweet"} -> <<"retweet">>;
        _               -> Default
    end.



%%--------------------------------------------------------------------
-spec get_comms_atom(Arg :: arg()) -> atom().
%%
% @doc  Returns the communications code requested in the YAWS `querydata';
%       or `undefined' if the comms code is not available.
% @end  --
get_comms_atom(Arg) ->
    get_comms(Arg, undefined).



%%--------------------------------------------------------------------
-spec get_comms_atom(Arg     :: arg(),
                     Default :: term()) -> atom().
%%
% @doc  Returns the communications code requested in the YAWS `querydata';
%       or the specified Default if the comms code is not available.
% @end  --
get_comms_atom(Arg, Default) ->
    Comms = get_comms(Arg, Default),
    if  is_atom(Comms)   -> Comms;
        is_binary(Comms) -> binary_to_existing_atom(Comms, utf8);
        is_list(Comms)   -> list_to_existing_atom(Comms)
    end.



%%--------------------------------------------------------------------
-spec get_conf() -> yaws_conf().
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

    ?debug("YAWS: id[~s]",   [Conf#yaws_conf.id]),
    %debug("YAWS: glob[~p]", [Conf#yaws_conf.gConf]),
    %debug("YAWS: srvs[~p]", [Conf#yaws_conf.sConfs]),
    %debug("YAWS: chSp[~p]", [Conf#yaws_conf.childSpecs]),
    Conf.



%%--------------------------------------------------------------------
-spec get_graph_dir() -> string().
%%
% @doc  Reports the directory where the WUI expects charts and graphs.
% @end  --
get_graph_dir() ->
    ?GRAPH_DIR.



%%--------------------------------------------------------------------
-spec get_reports(Arg :: arg()
                       | atom()
                       | string()) -> {reports(), reports()}.
%%
% @doc  Returns a double tuple with the Big Player Report and the
%       Regular Player Report as indicated by `raven'.
%
%       FIXME: We're currently only handling report periods of a day
% @end  --
get_reports(Arg = #arg{}) ->
    get_reports(get_track(Arg));


get_reports(undefined) ->
    EmptyRpt = #report{},
    EmptySet = [{full,    EmptyRpt},
                {tweet,   EmptyRpt},
                {retweet, EmptyRpt}],
    {EmptySet, EmptySet};


get_reports(Track) ->
    StatDir = wui:get_status_dir(),
    FileTag = wui:get_tag(Track),
    {ok, RptBin} = file:read_file(io_lib:format("~s/~s.report.etf", [StatDir, FileTag])),
    RptMap = binary_to_term(RptBin),
    {maps:get(big, RptMap, #report{}),
     maps:get(reg, RptMap, #report{})}.



%%--------------------------------------------------------------------
-spec get_status_dir() -> string().
%%
% @doc  Reports the directory where the WUI expects status and
%       statistical information.
% @end  --
get_status_dir() ->
    ?STATUS_DIR.



%%--------------------------------------------------------------------
-spec get_tag(Arg :: arg()
                   | atom()
                   | string()) -> string().
%%
% @doc  Returns the naming tag as track.percent.period.
% @end  --
get_tag(Arg = #arg{}) ->
    get_tag(get_track(Arg));


get_tag(Track) ->
    get_tag(Track, ?DEFAULT_BIG_P100, ?DEFAULT_PERIOD).



%%--------------------------------------------------------------------
-spec get_tag(Track   :: atom() | string(),
              BigP100 :: float(),
              Period  :: atom() | string()) -> string().
%%
% @doc  Returns the naming tag as track.percent.period.
% @end  --
get_tag(Track, BigP100, Period) ->
    io_lib:format("~s.~B.~s", [Track, round(100 * BigP100), Period]).



%%--------------------------------------------------------------------
-spec get_track(Arg :: arg()) -> binary()
                               | undefined.
%%
% @doc  Returns the tracking code requested in the URL `querydata'.
% @end  --
get_track(Arg) ->
    get_track(Arg, binary).


%%--------------------------------------------------------------------
-spec get_track(Arg  :: arg(),
                Type :: atom | binary | atom | string) -> atom()
                                                        | binary()
                                                        | string().
%%
% @doc  Returns the tracking code requested in the URL `querydata'
%       in the form of the specified data type.
% @end  --
get_track(Arg, Type) ->
    case yaws_api:queryvar(Arg, "track") of
        {ok, Text} ->

            % Yaws gives us the value as a string (list); convert as needed
            case {lists:member(Text, ?GOOD_TRACKS), Type} of

                {false, _}  -> undefined;
                {_, atom}   -> list_to_existing_atom(Text);
                {_, binary} -> list_to_binary(Text);
                {_, string} -> Text
            end;

        _ -> undefined
    end.



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
