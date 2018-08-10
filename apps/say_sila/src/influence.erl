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
         get_models/1,
         report_open/2,
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
%define(DELTA_CUTS,     [0.1, 0.2]).            % Sequence of parameter cuts
%define(DELTA_CUTS,     [0.1]).
-define(DELTA_CUTS,     []).
-define(INIT_BIG_PCT,   0.01).
-define(INIT_TOP_N,     10).
-define(INIT_METHOD,    {biggies, ?INIT_BIG_PCT}).


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
    start_link(Tracker, RunTag, RegComm, RegEmo, []).



%%--------------------------------------------------------------------
-spec start_link(Tracker :: cc | gw,
                 RunTag  :: stringy(),
                 RegComm :: comm_code(),
                 RegEmo  :: atom(),
                 Options :: proplist()) -> gen_statem:start_ret().
%%
% @doc  Startup function for Twitter influence model
% @end  --
start_link(Tracker, RunTag, RegComm, RegEmo, Options) ->
    gen_statem:start_link(?MODULE,
                          [Tracker, RunTag, RegComm, RegEmo, Options],
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
-spec get_models(Model :: gen_statem:server_ref()) -> queue:queue().
%%
% @doc  Returns a queue of historical models for this FSM.
% @end  --
get_models(Model) ->
    gen_statem:call(Model, get_models).



%%--------------------------------------------------------------------
-spec report_open(Name :: stringy(),
                  Opts :: proplist()) -> {ok, file:io_device(), string()}
                                       | {error, file:posix() | badarg | system_limit}.
%%
% @doc  Opens a report file and writes a CSV header with the
%       column names for a report on influence as modelled by
%       this FSM.
% @end  --
report_open(Name, Opts) ->
    Attrs = init_attributes(),
    report_open(Name, Attrs, Opts).



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
init([Tracker, RunTag, RegComm, RegEmo, Options]) ->

    process_flag(trap_exit, true),
    Players = player:get_players(Tracker),
    Name    = ?bin_fmt("~s_~s_~s_~s", [Tracker, RunTag, RegComm, RegEmo]),
    Attrs   = init_attributes(),
    Period  = proplists:get_value(period, Options, 1),

    %?debug("Influence for ~s: ~p", [Name, Options]),
    Biggies = case proplists:get_value(method, Options, ?INIT_METHOD) of
                  {biggies, Pct} -> player:get_biggies(Tracker, Pct);
                  {top_n,   N}   -> player:get_top_n(Tracker, N)
              end,

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

    {keep_state, Data#data{work_ref = WorkRef,
                           results  = none}};


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
eval(enter, _OldState, Data = #data{results = Results = #{status := ack},
                                    models  = Models}) ->
    %
    % Look at parameters to see what we should keep|cut
    eval_params(Data),
    {next_state, eval, Data#data{delta_cnt = 0,
                                 models    = queue:in(Results, Models)}};



eval(enter, _OldState, #data{name    = Name,
                             results = Results = #{status := nak}}) ->
    % Our modeller is unhappy, bail here!
    ?warning("Model ~s failed: alg[~p] info[~s]", [Name,
                                                   maps:get(model, Results, unknown),
                                                   maps:get(info,  Results, unknown)]),
    keep_state_and_data;


eval(cast, {eval_param, ready}, Data = #data{delta_cnt  = 0,
                                             delta_cuts = DeltaCuts,
                                             name       = Name,
                                             results    = Results}) ->
    
    % No parameters to cut, try the next cut level...
    case DeltaCuts of
        % We're all out of cuts, so we're done
        [] -> 
            ?notice("Completed processing for model ~s: corr[~7.4f]",
                    [Name, maps:get(correlation, Results, 0.0)]),

            {next_state, done, Data};

        % We've got a new cut to try, so set up for the next run...
        [Cut | RestCuts] ->
            NewData = Data#data{delta_cut  = Cut,
                                delta_cuts = RestCuts},
            eval_params(NewData),
            {next_state, eval, NewData}
    end;


eval(cast, {eval_param, ready}, Data = #data{delta_cnt = DeltaCnt,
                                             name      = Name}) ->
    %
    ?info("Model ~s finished updating parameters (rerunning): deltas[~B]",
          [Name, DeltaCnt]),
    {next_state, run, Data};


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
-spec eval_params(Data :: data()) -> ok.
%%
% @doc  Determines which of the current set of parameters should stay
%       the model.
% @end  --
eval_params(#data{name      = Name,
                  delta_cut = DeltaCut,
                  results   = #{coefficients := Coeffs,
                                correlation  := Score}}) ->

    ?notice("Evaluating model ~s: corr[~7.4f] cut[~f]", [Name, Score, DeltaCut]),
    FSM = self(),
    Caster = fun(Param) ->
                 gen_statem:cast(FSM, {eval_param, Param})
                 end,
    lists:foreach(Caster, maps:keys(Coeffs)),

    % Signal model run start|finalization
    Caster(ready).



%%--------------------------------------------------------------------
-spec report_open(Name  :: stringy(),
                  Attrs :: [atom()],
                  Opts  :: proplist()) -> {ok, file:io_device(), string()}.
%%
% @doc  Opens a report file and writes a CSV header with the
%       column names for a report on influence as modelled by
%       this FSM.
% @end  --
report_open(Name, Attrs, Opts) ->

    % Select heading text based on the report
    LineTag = case proplists:get_value(method, Opts, ?INIT_METHOD) of
                  {top_n,_,_} -> <<"N">>;
                  _           -> <<"run">>
              end,

    FPath = ioo:make_fpath(?REPORT_DIR, Name, <<"csv">>),
    {ok, FOut} = file:open(FPath, [write]),

    % Create a CSV header with all possible model attributes
    ?io_fmt(FOut, "~s,tracker,reg_comm,reg_emo,Correlation,samples", [LineTag]),
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
