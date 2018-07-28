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
         eval_param/2,
         get_models/1,
         report_open/1,
         report_line/3]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([idle/3,
         run/3,
         eval/3,
         done/3]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


-define(REPORT_DIR,     ?WORK_DIR "/influence").
-define(ATTR_DTS,       minute).                % Date-timestamp attribute
-define(EXCLUDED_ATTRS, [?ATTR_DTS]).           % Attributes to exclude from ARFF
-define(EPSILON,        0.00000001).            % Floating point comparisons
-define(DELTA_CUTS,     [?EPSILON, 0.1, 0.2]).  % Sequence of parameter cuts
%define(DELTA_CUTS,     [?EPSILON]).


%%--------------------------------------------------------------------
-record(data, {tracker                  :: tracker(),
               name                     :: binary(),
               arff                     :: binary(),
               attributes               :: [atom()],
               incl_attrs               :: [atom()],            % Info-purposes only
               excl_attrs               :: [atom()],            % Info-purposes only
               reg_comm                 :: comm_code(),
               reg_emo                  :: emotion(),
               period                   :: non_neg_integer(),
               players                  :: map(),
               biggies                  :: proplist(),
               jvm_node                 :: atom(),
               delta_cuts = ?DELTA_CUTS :: [float()],           % Min param values for future passes
               delta_cut  = ?EPSILON    :: float(),             % Min param value for current model
               delta_cnt  = 0           :: non_neg_integer(),   % Num changes since last evaluation
               work_ref   = none        :: none|reference(),
               results    = none        :: none|map(),
               models                   :: queue:queue()}).
-type data() :: #data{}.                                        % FSM internal state data



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: cc | gw,
                 RunTag  :: stringy(),
                 RegComm :: comm_code(),
                 RegEmo  :: atom()) -> gen_statem:start_ret().
%%
% @doc  Startup function for a daily Twitter influence model
% @end  --
start_link(Tracker, RunTag, RegComm, RegEmo) ->
    start_link(Tracker, RunTag, RegComm, RegEmo, 1).



%%--------------------------------------------------------------------
-spec start_link(Tracker :: cc | gw,
                 RunTag  :: stringy(),
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



%%--------------------------------------------------------------------
-spec get_models(Model :: gen_statem:server_ref()) -> queue:queue().
%%
% @doc  Returns a queue of historical models for this FSM.
% @end  --
get_models(Model) ->
    gen_statem:call(Model, get_models).



%%--------------------------------------------------------------------
-spec report_open(Name :: stringy()) -> {ok, file:io_device(), string()}
                                      | {error, file:posix() | badarg | system_limit}.
%%
% @doc  Opens a report file and writes a CSV header with the
%       column names for a report on influence as modelled by
%       this FSM.
% @end  --
report_open(Name) ->
    Attrs = init_attributes(),
    report_open(Name, Attrs).



%%--------------------------------------------------------------------
-spec report_line(Model :: gen_statem:server_ref(),
                  FOut  :: file:io_device(),
                  Line  ::  non_neg_integer()) -> ok.
%%
% @doc  Writes out a CSV line describing the final version of the
%       model this FSM represents.
%
%       NOTE: If the FSM is still processing models, this function
%             will block until the final model is processed.
% @end  --
report_line(Model, FOut, Line) ->
    gen_statem:call(Model, {report_line, FOut, Line}).



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
    Attrs   = init_attributes(),

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
                     attributes = Attrs,
                     incl_attrs = Attrs,
                     excl_attrs = [?ATTR_DTS],
                     reg_comm   = RegComm,
                     reg_emo    = RegEmo,
                     period     = Period,
                     players    = Players,
                     biggies    = Biggies,
                     jvm_node   = raven:get_jvm_node(Tracker),
                     models     = queue:new()}}.



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
handle_event({call, From}, get_models, Data = #data{models = Models}) ->
    {keep_state, Data, [{reply, From, Models}]};


handle_event({call, _From}, {report_line, _, Line}, Data = #data{name = Name}) ->
    ?debug("Waiting for report line #~B: ~s", [Line, Name]),
    {keep_state, Data, [postpone]};


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
                                 delta_cut  = ?EPSILON,
                                 delta_cuts = ?DELTA_CUTS,
                                 work_ref   = none,
                                 results    = none,
                                 models     = queue:new()}};


handle_event(cast, go, #data{name = Name}) ->
    ?warning("Model ~s got the go-ahead but is already under way", [Name]),
    keep_state_and_data;


handle_event(Type, Evt, #data{name = Name}) ->
    ?warning("Model ~s received an unexpected '~p' event: type[~p]", [Name, Evt, Type]),
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
eval(enter, _OldState, Data = #data{name       = Name,
                                    delta_cuts = DeltaCuts,
                                    results    = #{status       := ack,
                                                   coefficients := Coeffs,
                                                   correlation  := Correlation}}) ->

    % We're going to be sending one or more events to ourself...
    FSM = self(),

    % Do we have cuts to make?
    {DeltaCut,
     RestCuts} = case DeltaCuts of
        []          -> {Data#data.delta_cut, []};
        [Cut|Rest]  -> {Cut, Rest}
    end,

    ?notice("Evaluating model ~s: corr[~7.4f] cut[~f]", [Name, Correlation, DeltaCut]),
    lists:foreach(fun(P) -> eval_param(FSM, P) end,
                  maps:keys(Coeffs)),
    eval_param(FSM, ready),                             % Signal model run start|finalization
    {next_state, eval, Data#data{delta_cnt  = 0,
                                 delta_cut  = DeltaCut,
                                 delta_cuts = RestCuts}};


eval(enter, _OldState, #data{name    = Name,
                             results = Results = #{status := nak}}) ->
    %
    ?warning("Model ~s failed: alg[~p] info[~s]", [Name,
                                                   maps:get(model, Results, unknown),
                                                   maps:get(info,  Results, unknown)]),
    keep_state_and_data;


eval(cast, {eval_param, ready}, Data = #data{name    = Name,
                                             results = Results,
                                             models  = Models}) ->
    %
    {NextState,
     NextResults} = case Data#data.delta_cnt of
        % No deltas means we're done
        0 ->
            ?notice("Completed processing for model ~s: corr[~7.4f]",
                    [Name, maps:get(correlation, Results, 0.0)]),
            {done, Results};
        % We've made changes, time to rerun the model
        DeltaCnt ->
            ?info("Model ~s finished updating parameters (rerunning): deltas[~B]",
                  [Name, DeltaCnt]),
            {run, none}
    end,
    {next_state, NextState,  Data#data{results = NextResults,
                                       models  = queue:in(Results, Models)}};


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
            case abs(Coeff) >= DeltaCut of
                true  ->
                    ?debug("Keeping parameter: ~s", [Param]),
                    {InclAttrs, ExclAttrs, DeltaCnt};

                false ->
                    ?info("Cutting parameter ~s for model ~s: coeff[~f]", [Param, Name, Coeff]),
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



%%--------------------------------------------------------------------
-spec done(Type :: doner|gen_statem:event_type(),
           Evt  :: term(),
           Data :: data()) -> gen_statem:state_enter_result(done)
                            | gen_statem:state_function_result(done).
%%
% @doc  FSM state to finalize the FSM's model series and create the
%       associated report.
% @end  --
done(enter, _OldState, Data = #data{name = Name}) ->
    %
    {RptStat,
     RptFPath} = report_all(Data),
    ?info("Reported ~s created: path[~s] stat[~p]", [Name, RptFPath, RptStat]),
    keep_state_and_data;


done({call, From}, {report_line, FOut, Line}, Data = #data{name       = Name,
                                                           attributes = Attrs,
                                                           results    = Results}) ->
    %
    % The caller wants just one line for our final report
    ?info("Report line #~B requested: ~s", [Line, Name]),
    report_line(Results, {FOut, Line, Attrs, Data}),

    {keep_state_and_data, [{reply, From, ok}]};


done(Type, Evt, Data) ->
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



%%--------------------------------------------------------------------
-spec report_open(Name  :: stringy(),
                  Attrs :: [atom()]) -> {ok, file:io_device(), string()}.
%%
% @doc  Opens a report file and writes a CSV header with the
%       column names for a report on influence as modelled by
%       this FSM.
% @end  --
report_open(Name, Attrs) ->

    FPath = ioo:make_fpath(?REPORT_DIR, Name, <<"csv">>),
    {ok, FOut} = file:open(FPath, [write]),

    % Create a CSV header with all possible model attributes
    ?io_put(FOut, "run,tracker,reg_comm,reg_emo,Correlation,samples"),
    lists:foreach(fun(A) -> ?io_fmt(FOut, ",~s", [A]) end, Attrs),
    ?io_put(FOut, ",intercept\n"),

    {ok, FOut, FPath}.



%%--------------------------------------------------------------------
-spec report_all(Data :: data()) -> {ok, string()}
                                  | {{error, atom()}, string()}.
%%
% @doc  Writes a CSV report for the full set of model runs for this FSM.
% @end  --
report_all(Data = #data{name       = Name,
                        models     = Models,
                        attributes = Attrs}) ->

    {ok, FOut, FPath} = report_open(Name, Attrs),

    % Fill in the CSV, one line per model run result
    lists:foldl(fun report_line/2, {FOut, 0, Attrs, Data}, queue:to_list(Models)),

    % And we're done!
    FStatus = file:close(FOut),
    {FStatus, FPath}.



%%--------------------------------------------------------------------
-spec report_line(Results :: map(),
                  Acc     :: {FOut  :: file:io_device(),
                              Line  :: non_neg_integer(),
                              Attrs :: [atom()],
                              Data  :: data()}) -> {file:io_device(),
                                                    non_neg_integer(),
                                                    [atom()],
                                                    data()}.
%%
% @doc  Writes one line of a CSV report for the specified results.
% @end  --
report_line(#{coefficients := Coeffs,
              intercept    := Intercept,
              correlation  := CorrScore,
              instances    := InstCnt},
            {FOut, Line, Attrs, Data = #data{tracker  = Tracker,
                                             reg_comm = RegComm,
                                             reg_emo  = RegEmo}}) ->

    Attribber = fun(Attr) ->
                    case maps:get(Attr, Coeffs, no_param) of
                        no_param -> ?io_put(FOut, ",");
                        Coeff    -> ?io_fmt(FOut, ",~f", [Coeff])
                    end,
                    Coeffs end,

    ?io_fmt(FOut, "~B,~s,~s,~s,~f,~B",
            [Line, Tracker, RegComm, RegEmo, CorrScore,InstCnt]),
    lists:foreach(Attribber, Attrs),
    ?io_fmt(FOut, ",~f~n", [Intercept]),

    % Only the line number changes in the accumulator
    {FOut, Line+1, Attrs, Data}.
