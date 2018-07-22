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
         go/1,
         eval_param/2]).        % DEBUG!
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


-define(EPSILON,        0.00000001).            % Floating point comparisons
-define(ATTR_DTS,       minute).                % Date-timestamp attribute
-define(EXCLUDED_ATTRS, [?ATTR_DTS]).           % Attributes to exclude from ARFF


%%--------------------------------------------------------------------
-record(data, {tracker          :: tracker(),
               name             :: binary(),
               arff             :: binary(),
               incl_attrs       :: [atom()],
               excl_attrs       :: [atom()],
               reg_comm         :: comm_code(),
               reg_emo          :: emotion(),
               period           :: non_neg_integer(),
               players          :: map(),
               biggies          :: proplist(),
               jvm_node         :: atom(),
               delta_cnt = 0    :: non_neg_integer(),   % Num. changes to model since last evaluation
               delta_cut = 0.0  :: float(),             % Minimum parameter value to be included in model
               work_ref  = none :: none|reference(),
               results   = none :: none|map() }).
-type data() :: #data{}.                                % FSM internal state data



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



%%--------------------------------------------------------------------
-spec eval_param(Model :: gen_statem:server_ref(),
                 Param :: atom()) -> ok.
%%
% @doc  Determines whether or not the specified parameter belongs in
%       the model.
%
%       NOTE: This function is meant to be called from within the
%             FSM code.  It is exported for debugging purposes.
% @end  --
eval_param(Model, Param) ->
    gen_statem:cast(Model, {eval_param, Param}).



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
    Name    = ?bin_fmt("~s_~s_~s_~s", [Tracker, RunTag, RegComm, RegEmo]),

    BigInfo = fun({Comm, {P100, Cnt, Accts}}) ->
                  ?str_FMT("{~s:~B%,tw=~B,cnt=~B}", [Comm, round(100 * P100), Cnt, length(Accts)])
                  end,

    ?info("Modelling '~s' influence: usr[~B] big~p", [Name,
                                                      maps:size(Players),
                                                      [BigInfo(Grp) || Grp <- Biggies]]),

    {ok, ARFF} = weka:biggies_to_arff(Name, RegComm, RegEmo, Biggies, Players, Period),

    {ok, idle, #data{tracker    = Tracker,
                     name       = Name,
                     arff       = list_to_binary(ARFF),
                     incl_attrs = init_attributes(),
                     excl_attrs = [?ATTR_DTS],
                     reg_comm   = RegComm,
                     reg_emo    = RegEmo,
                     period     = Period,
                     players    = Players,
                     biggies    = Biggies,
                     jvm_node   = raven:get_jvm_node(Tracker)}}.




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
    %
    ?notice("Resetting model ~s", [Name]),
    case Data#data.work_ref of
        none -> ok;
        Work -> ?warning("Abandoning ourstanding model: ~p", [Work])
    end,
    {next_state, idle, Data#data{incl_attrs = init_attributes(),
                                 excl_attrs = ?EXCLUDED_ATTRS,
                                 delta_cnt  = 0,
                                 work_ref   = none}};


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



%%-------------------------------------------------------------------
%% run:
%%
% @doc  FSM state for running a Weka model
% @end  --
run(enter, _OldState, Data = #data{name       = Name,
                                   arff       = ARFF,
                                   excl_attrs = ExclAttrs,
                                   jvm_node   = JVM}) ->

    ?info("Model ~s running on Weka", [Name]),
    WorkRef = case Data#data.work_ref of
        none   -> ok;
        OldRef ->
            ?warning("Running a model with another outstanding: old[~p]", [OldRef]),
            make_ref()
    end,
    % The Weka filter takes a regular expression to filter attributes by name
    ExclRE = lists:flatten(?str_fmt("~s", [hd(ExclAttrs)]) ++
                           [?str_fmt("|~s", [Attr]) || Attr <- tl(ExclAttrs)]),

    ?debug("Attribute filter: ~s", [ExclRE]),
    {say, JVM} ! {self(), WorkRef, regress, jsx:encode(#{arff    => ARFF,
                                                         exclude => list_to_binary(ExclRE)})},

    {keep_state, Data#data{work_ref = WorkRef}};


run(info, {From, WorkRef, regress, Results}, Data = #data{name = Name}) ->
    %
    case Data#data.work_ref of
        WorkRef ->
            % Yay, these are the results we're waiting on!
            ?info("Model ~s results from ~p", [Name, From]),
            {next_state, eval, Data#data{work_ref = none,
                                         results  = Results}};
        _ ->
            % We've got a work mismatch. Stay put...
            % We'll either get the right work, or a human can reset the FSM.
            ?warning("Unexpected results for model ~s: src[~p] ref[~p]",
                     [Name, From, WorkRef]),
            keep_state_and_data
    end;


run(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%--------------------------------------------------------------------
-spec eval(Type :: enter|gen_statem:event_type(),
           Evt  :: term(),
           Data :: data()) -> gen_statem:state_enter_result(eval)
                            | gen_statem:state_function_result(eval).
%%
% @doc  FSM state to evaluate a Weka model
% @end  --
eval(enter, _OldState, Data = #data{name    = Name,
                                    results = #{status       := ack,
                                                coefficients := Coeffs,
                                                correlation  := Correlation}}) ->
    %
    ?notice("Evaluating model ~s: corr[~7.4f]", [Name, Correlation]),
    FSM = self(),
    lists:foreach(fun(P) -> eval_param(FSM, P) end,
                  maps:keys(Coeffs)),
    eval_param(FSM, ready),
    {next_state, eval, Data#data{delta_cnt = 0,
                                 delta_cut = ?EPSILON}};


eval(enter, _OldState, #data{name    = Name,
                             results = Results = #{status := nak}}) ->
    %
    ?warning("Model ~s failed: alg[~p] info[~s]", [Name,
                                                   maps:get(model, Results, unknown),
                                                   maps:get(info,  Results, unknown)]),
    keep_state_and_data;


eval(cast, {eval_param, ready}, Data = #data{name    = Name,
                                             results = Results}) ->
    %
    NextState = case Data#data.delta_cnt of
        % No deltas means we're done
        0 ->
            ?notice("Completed processing for model ~s: corr[~7.4f]",
                    [Name, maps:get(correlation, Results, 0.0)]),
            eval;
        % We've made changes, time to rerun the model
        DeltaCnt ->
            ?info("Model ~s finished updating parameters (rerunning): deltas[~B]",
                  [Name, DeltaCnt]),
            run
    end,
    {next_state, NextState,  Data};



eval(cast, {eval_param, Param}, Data = #data{name       = Name,
                                             incl_attrs = InclAttrs,
                                             excl_attrs = ExclAttrs,
                                             delta_cnt  = DeltaCnt,
                                             delta_cut  = DeltaCut,
                                             results    = #{coefficients := Coeffs}}) ->
    %?debug("PARAM: ~s: ~p", [Param, Coeffs]),
    {NewInclAttrs,
     NewExclAttrs,
     NewDeltaCnt} = case maps:get(Param, Coeffs, undefined) of

        undefined ->
            ?warning("Unknown parameter: ~p", [Param]),
            {InclAttrs, ExclAttrs, DeltaCnt};

        Coeff ->
            case Coeff >= DeltaCut of
                true  ->
                    ?debug("Keeping parameter: ~s", [Param]),
                    {InclAttrs, ExclAttrs, DeltaCnt};

                false ->
                    ?info("Cutting parameter ~s for model ~s: coeff[~7.4f]", [Param, Name, Coeff]),
                    {lists:delete(Param, InclAttrs),
                     [Param | ExclAttrs],
                     DeltaCnt + 1}
            end
    end,
    {next_state, eval, Data#data{incl_attrs = NewInclAttrs,
                                 excl_attrs = NewExclAttrs,
                                 delta_cnt  = NewDeltaCnt}};

eval(Type, Evt, Data) ->
    handle_event(Type, Evt, Data).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec init_attributes() -> [atom()].
%%
% @doc  Initializes the independent attribute list.
% @end  --
init_attributes() ->
    [weka:make_attribute(big, Code, Emo) || Code <- weka:get_big_comm_codes(),
                                            Emo  <- ?EMOTIONS].

