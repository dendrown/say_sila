%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc FSM to model influence using Weka.
%%
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(influence).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-behaviour(gen_statem).
-export([start_link/4, start_link/5,
         stop/1,
         reset/1,
         go/1]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([idle/3,
         run/3,
         eval/3]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").



%%--------------------------------------------------------------------
-record(data, {tracker    :: tracker(),
               name       :: string(),
               attributes :: [string()],
               reg_comm   :: comm_code(),
               reg_emo    :: emotion(),
               period     :: non_neg_integer(),
               players    :: map(),
               biggies    :: proplist() }).
%type data() :: #data{}.                        % FSM internal state data



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: cc | gw,
                 RunTag  :: string(),
                 RegComm :: comm_code(),
                 RegEmo  :: atom()) -> gen_statem:start_ret().
%%
% @doc  Startup function for a daily Twitter influence model
% @end  --
start_link(Tracker, RunTag, RegComm, RegEmo) ->
    start_link(Tracker, RunTag, RegComm, RegEmo, 1).



%%--------------------------------------------------------------------
-spec start_link(Tracker :: cc | gw,
                 RunTag  :: string(),
                 RegComm :: comm_code(),
                 RegEmo  :: atom(),
                 Period  :: non_neg_integer()) -> gen_statem:start_ret().
%%
% @doc  Startup function for Twitter influence model
% @end  --
start_link(Tracker, RunTag, RegComm, RegEmo, Period) ->
    gen_statem:start_link(?MODULE,
                          [Tracker, RunTag, RegComm, RegEmo, Period],
                          []).



%%--------------------------------------------------------------------
-spec stop(Model :: gen_statem:server_ref()) -> ok.
%%
% @doc  Shutdown the state machine.
% @end  --
stop(Model) ->
    gen_statem:call(Model, cancel),
    gen_statem:stop(Model).



%%--------------------------------------------------------------------
-spec reset(Model :: gen_statem:server_ref()) -> ok.
%%
% @doc  Resets the modeller
% @end  --
reset(Model) ->
    gen_statem:cast(Model, reset).



%%--------------------------------------------------------------------
-spec go(Model :: gen_statem:server_ref()) -> ok.
%%
% @doc  Gets the influence model running
% @end  --
go(Model) ->
    gen_statem:cast(Model, go).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the back-off timer.
% @end  --
init([Tracker, RunTag, RegComm, RegEmo, Period]) ->

    process_flag(trap_exit, true),
    Players = player:get_players(Tracker),
    Biggies = player:get_biggies(Tracker, 0.01),
    Name    = ?str_FMT("~s_~s_~s_~s", [Tracker, RunTag, RegComm, RegEmo]),

    BigInfo = fun({Comm, {P100, Cnt, Accts}}) ->
                  ?str_FMT("{~s:~B%,tw=~B,cnt=~B}", [Comm, round(100 * P100), Cnt, length(Accts)])
                  end,

    ?info("Modelling '~s' influence: usr[~B] big~p", [Name,
                                                      maps:size(Players),
                                                      [BigInfo(Grp) || Grp <- Biggies]]),
    {ok, idle, #data{tracker    = Tracker,
                     name       = Name,
                     attributes = init_attributes(),
                     reg_comm   = RegComm,
                     reg_emo    = RegEmo,
                     period     = Period,
                     players    = Players,
                     biggies    = Biggies}}.




%%--------------------------------------------------------------------
%% callback_mode:
%%
% @doc  Reports FSM callback mode.
% @end  --
callback_mode() ->
    [state_functions, state_enter].



%%--------------------------------------------------------------------
%% terminate:
%%
% @doc  FSM shutdown callback.
% @end  --
terminate(Why, _State, _Data) ->
    ?info("Model shutdown: why[~p]", [Why]),
    normal.



%%--------------------------------------------------------------------
%% code_change:
%%
% @doc  Hot code update processing: a placeholder.
% @end  --
code_change(OldVsn, _State, Data, _Extra) ->
    ?notice("Hot code update: old[~p]", [OldVsn]),
    {ok, Data}.



%%--------------------------------------------------------------------
%% handle_event:
%%
% @doc  Synchronous messages for Coinigy services
% @end  --
handle_event(cast, reset, Data = #data{name = Name}) ->
    ?notice("Resetting model ~s", [Name]),
    {next_state, idle, Data#data{attributes = init_attributes()}};


handle_event(cast, go, #data{name = Name}) ->
    ?warning("Model ~s got the go-ahead but is already under way", [Name]),
    keep_state_and_data;


handle_event(_, Evt, #data{name = Name}) ->
    ?warning("Model ~s received an unexpected event: ~p", [Name, Evt]),
    keep_state_and_data.



%%--------------------------------------------------------------------
%% idle:
%%
% @doc  FSM state waiting for something to do
% @end  --
idle(enter, _OldState, #data{name = Name}) ->
    %
    ?info("Model ~s is ready and waiting", [Name]),
    keep_state_and_data;


idle(cast, go, Data) ->
    {next_state,
     run,
     Data};

idle(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%--------------------------------------------------------------------
%% run:
%%
% @doc  FSM state for running a Weka model
% @end  --
run(enter, _OldState, #data{name     = Name,
                            reg_comm = RegComm,
                            reg_emo  = RegEmo,
                            period   = Period,
                            players  = Players,
                            biggies  = Biggies}) ->

    ?info("Model ~s running on Weka", [Name]),
    weka:biggies_to_arff(Name, RegComm, RegEmo, Biggies, Players, Period),
    keep_state_and_data;

run(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%--------------------------------------------------------------------
%% eval:
%%
% @doc  FSM state to evaluate a Weka model
% @end  --
eval(enter, _OldState, #data{name = Name}) ->
    %
    ?info("Model ~s under evaluation", [Name]),
    keep_state_and_data;


eval(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec init_attributes() -> [string()].
%%
% @doc  Initializes the independent attribute list.
% @end  --
init_attributes() ->
    BigCommCodes = weka:get_big_comm_codes(),
    [?str_FMT("big_~s_~s", [C,E]) || C <- BigCommCodes,
                                     E <- ?EMOTIONS].
