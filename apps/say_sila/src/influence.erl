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
%%                 +------------+
%%      (init)---->|    idle    |
%%                 +------------+
%%                       |
%%                     go|
%%                       |
%%                       v
%%                 +------------+   |
%%                 |    run     |---+
%%                 +------------+
%%                    |     ^
%%             regress|     |
%%                    |     |eval_param
%%                    v     |
%%                 +------------+
%%                 |    eval    |
%%                 +------------+
%%                       |
%%             eval_param|
%%                       |
%%                       v
%%                 +------------+
%%                 |    done    |
%%                 +------------+
%%
%% @copyright 2018-2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(influence).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-behaviour(gen_statem).
-export([start_link/4,  start_link/5,
         stop/1,
         reset/1,
         go/1,
         get_models/1,
         get_outcome/1,
         init_params/1, init_params/2,  init_params/3,
         init_range/1,
         report_open/2,
         report_line/3,
         run_biggies/2, run_biggies/3,  run_biggies/4,
         run_top_n/3,   run_top_n/4,
         run_top_nn/3,  run_top_nn/4,   run_top_nn/5]).

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
-define(EVAL_METHOD,    cv).                    % Evaluate using cross-validation
%define(EVAL_METHOD,    data).                  % Evaluate using multiple datasets
-define(INIT_PERIOD,    7).                     % Default to a week
-define(INIT_BIG_PCT,   0.004).                 % Contribution rate (without deceleration)
-define(INIT_TOP_N,     10).
-define(INIT_METHOD,    {biggies, ?INIT_BIG_PCT}).

-define(MIN_BIG_PCT,    0.001).
-define(MAX_BIG_PCT,    0.004).
-define(INC_BIG_PCT,    0.0005).
-define(INIT_BIG_RANGE, {biggies, ?MIN_BIG_PCT, ?MAX_BIG_PCT}).

-define(MIN_TOP_N,      5).
%define(MIN_TOP_N,      24).                    % FIXME: Developing strategy for medium players
-define(MAX_TOP_N,      25).
-define(MIN_NN_SCORE,   0.6).
-define(INIT_TOP_RANGE, {top_n, ?MIN_TOP_N, ?MAX_TOP_N}).


%%--------------------------------------------------------------------
-record(data, {tracker                  :: tracker(),
               name                     :: binary(),
               datasets                 :: map(),
               work_csvs                :: map(),               % Weka can save filtered data for incanter
               attributes               :: [atom()],
               init_attrs               :: [atom()],
               incl_attrs               :: [atom()],
               excl_attrs               :: [atom()],
               reg_comm                 :: comm_code(),
               reg_emo                  :: emotion(),
               period                   :: pos_integer(),
               biggies                  :: proplist(),
               jvm_node                 :: atom(),
               delta_cuts = ?DELTA_CUTS :: [float()],           % Min param values for future passes
               delta_cut  = ?EPSILON    :: float(),             % Min param value for current model
               delta_cnt  = 0           :: non_neg_integer(),   % Num changes since last evaluation
               work_ref   = none        :: none|reference(),
               results    = none        :: none|map(),
               models                   :: queue:queue(),
               report                   :: boolean()}).
-type data()    :: #data{}.                                     % FSM internal state data

-type method()       :: biggies | top_n.
-type method_range() :: {biggies, float(), float()}
                      | {top_n, pos_integer(), non_neg_integer()}.


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
                 Params  :: proplist()) -> gen_statem:start_ret().
%%
% @doc  Startup function for Twitter influence model
% @end  --
start_link(Tracker, RunTag, RegComm, RegEmo, Params) ->

    % It's too easy to get the comm/emo backwards, and the model goes haywire!
    case {lists:member(RegComm, ?COMM_CODES),
          lists:member(RegEmo,  ?EMOTIONS)} of

        {true, true} ->
            gen_statem:start_link(?MODULE,
                                  [Tracker, RunTag, RegComm, RegEmo, Params],
                                  []);

        _ ->
            ?error("Invalid parameter: comm[~s] emo[~s]", [RegComm, RegEmo]),
            {stop, badarg}
    end.



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
-spec get_outcome(Model :: gen_statem:server_ref()) -> {float(),
                                                        [atom()]}.
%%
% @doc  Returns a tuple containing the correlation coefficient and
%       the attributes that were actually used in the final model.
% @end  --
get_outcome(Model) ->
    gen_statem:call(Model, get_outcome).



%%--------------------------------------------------------------------
-spec get_models(Model :: gen_statem:server_ref()) -> queue:queue().
%%
% @doc  Returns a queue of historical models for this FSM.
% @end  --
get_models(Model) ->
    gen_statem:call(Model, get_models).



%%--------------------------------------------------------------------
-spec init_params(Method :: method()) -> proplist().

-spec init_params(Method :: method(),
                  Params :: proplist()) -> proplist().

-spec init_params(Method :: method(),
                  Attrs  :: [atom()],
                  Params :: proplist()) -> proplist().
%%
% @doc  A shortcut for initializing model parameter lists.
% @end  --
init_params(Method) ->
    init_params(Method, []).


init_params(Method, Params) ->
    [{method, init_range(Method)} | Params].


init_params(Method, Attrs, Params) ->
    MethParams = init_params(Method, Params),
    case Attrs of
        [] -> MethParams;
        _  -> [{init_attrs, Attrs} | MethParams]
    end.



%%--------------------------------------------------------------------
-spec init_range(Method :: method()) -> method_range().
%%
% @doc  Returns the default options tuple to specify the range of N
%       for the Top-N big players.
% @end  --
init_range(Method) ->
    case Method of
        biggies -> ?INIT_BIG_RANGE;
        top_n   -> ?INIT_TOP_RANGE
    end.



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
-spec report_line(Model :: gen_statem:server_ref() | none,
                  FOut  :: file:io_device(),
                  Line  :: non_neg_integer()) -> ok.
%%
% @doc  Writes out a CSV line describing the final version of the
%       model this FSM represents.
%
%       NOTE: If the FSM is still processing models, this function
%             will block until the final model is processed.
% @end  --
report_line(none, FOut, Line) ->
    ?io_fmt(FOut, "~B,,,,,,,,,,,,,,,,,,,,,,~n", [Line]);


report_line(Model, FOut, Line) ->
    gen_statem:call(Model, {report_line, FOut, Line}).



%%--------------------------------------------------------------------
-spec run_biggies(Tracker :: tracker(),
                  RunTag  :: stringy()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments.  The caller must
%       specify the tracker, but we default to biggie velocity at
%       1% over weekly periods.
% @end  --
run_biggies(Tracker, RunTag) ->
    run_biggies(Tracker, RunTag, ?INIT_BIG_PCT).



%%--------------------------------------------------------------------
-spec run_biggies(Tracker :: tracker(),
                  RunTag  :: stringy(),
                  Emo_Pct :: emotion
                           | float()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments.  The caller must
%       specify the tracker, but we default to biggie velocity at
%       .4% over weekly periods.
% @end  --
%run_biggies(Tracker, RunTag, Emo) when is_atom(Emo) ->

run_biggies(Tracker, RunTag, BigPct) ->
    run_influence(Tracker, RunTag, [{method, {biggies, BigPct}},
                                    report]).



%%--------------------------------------------------------------------
-spec run_biggies(Tracker :: tracker(),
                  RunTag  :: stringy(),
                  Comm    :: comm_code(),
                  Emo     :: emotion()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments for one communication
%       code and one emotion.  The caller must specify the tracker, but
%       we default to biggie velocity at .4% over weekly periods.
% @end  --
run_biggies(Tracker, RunTag, Comm, Emo) ->
    BigTag  = extend_run_tag(RunTag, Comm, Emo, ?INIT_BIG_RANGE),
    run_influence(Tracker, BigTag, Comm, Emo, init_params(biggies)).



%%--------------------------------------------------------------------
-spec run_top_n(Tracker :: tracker(),
                RunTag  :: stringy(),
                Emo_N   :: emotion()
                         | non_neg_integer()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts in each
%       category.  This function defaults to weekly periods.
% @end  --
run_top_n(Tracker, RunTag, Emo) when is_atom(Emo) ->
    Comms  = [tter, oter, rter],
    Runner = fun(Comm) ->
                 Tag = ?str_fmt("~s_~s_~s", [RunTag, Comm, Emo]),
                 run_top_n(Tracker, Tag, Comm, Emo)
                 end,
    lists:map(Runner, Comms);


run_top_n(Tracker, RunTag, N) when is_integer(N) ->
    Params = [{method, {top_n, N}},
               {period, 7}],
    run_influence(Tracker, RunTag, Params).



%%--------------------------------------------------------------------
-spec run_top_n(Tracker :: tracker(),
                RunTag  :: stringy(),
                Comm    :: comm_code(),
                Emo     :: emotion()) -> proplist().

-spec run_top_n(Tracker :: tracker(),
                RunTag  :: stringy(),
                Comm    :: comm_code(),
                Emo     :: emotion(),
                Params  :: proplist()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This function defaults to weekly periods.
% @end  --
run_top_n(Tracker, RunTag, Comm, Emo) ->
    run_top_n(Tracker, RunTag, Comm, Emo, []).


run_top_n(Tracker, RunTag, Comm, Emo, Params) ->
    run_influence(Tracker, RunTag, Comm, Emo, init_params(top_n, Params)).



%%--------------------------------------------------------------------
-spec run_top_nn(Tracker   :: tracker(),
                 RunTag    :: stringy(),
                 CommOrEmo :: comm_code() | emotion()) -> ok.
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This `nn' function goes through the Top-N twice, using only
%       the emo/comm attributes from the higher ranking models.
%
%       This function runs weekly periods for either one of the
%       `tter', `oter' or `rter' communication codes, or an emotion.
% @end  --
run_top_nn(Tracker, RunTag, CommOrEmo) ->

    % We don't currently do this for passive communication categories
    ActComms = [tter, oter, rter],

    % Did they specify a comm code?
    case lists:member(CommOrEmo, ActComms) of

        % Run through all the emotions for the requested comm
        true ->
            lists:foreach(fun(Emo) -> run_top_nn(Tracker, RunTag, CommOrEmo, Emo) end,
                          ?EMOTIONS);

        % So, it should be an emotion...
        false ->
            case lists:member(CommOrEmo, ?EMOTIONS) of

                % Run through all active comm codes
                true ->
                    lists:foreach(fun(Comm) -> run_top_nn(Tracker, RunTag, Comm, CommOrEmo) end,
                                  ActComms);

                % We probably tried a passive communication category and forgot we don't support it
                false ->
                    ?warning("Unsupported communication code or emotion: ~p", [CommOrEmo])
            end
    end.



%%--------------------------------------------------------------------
-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy(),
                 Comm    :: comm_code(),
                 Emo     :: emotion()) -> proplist().

-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy(),
                 Comm    :: comm_code(),
                 Emo     :: emotion(),
                 Params  :: proplist()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This `nn' function goes through the Top-N twice, using only
%       the emo/comm attributes from the higher ranking models.
%
%       This function uses weekly periods.
% @end  --
run_top_nn(Tracker, RunTag, Comm, Emo) ->
    run_top_nn(Tracker, RunTag, Comm, Emo, []).


run_top_nn(Tracker, RunTag, Comm, Emo, Params) ->

    % Do An initial run, keeping the results above a minimum correlation score
    TagTopN  = extend_run_tag(RunTag, Comm, Emo, ?INIT_TOP_RANGE),
    MinScore = ?MIN_NN_SCORE,
    BaseRun  = lists:filter(fun({_, {Score, _}}) ->
                                abs(Score) >= MinScore
                                end,
                            run_top_n(Tracker, TagTopN, Comm, Emo, Params)),

    % Function to tally up attribute usage across the models
    Counter = fun Recur([], Cnts) ->
                      Cnts;
                  Recur([A|RestAttrs], Cnts) ->
                      % Add/update this attribute on the counts list
                      NewCnts = case sila:split_on_prop(A, Cnts) of
                          {undefined, _ } -> [{A, 1} | Cnts];
                          {Cnt, RestCnts} -> [{A, Cnt+1} | RestCnts]
                      end,
                      Recur(RestAttrs, NewCnts) end,

    % Keep the attributes that occur in at least half of the better models
    MinCount = round(length(BaseRun) / 2),
    TagTopNN = ?str_FMT("~s_NN", [TagTopN]),

    AttrCnts = lists:foldl(fun({_, {_, Attrs}}, Acc) -> Counter(Attrs, Acc) end,
                           [],
                           BaseRun),

    AttrsNN  = [Attr || {Attr, _} <- lists:filter(fun({_, Cnt}) -> Cnt >= MinCount end,
                                                  AttrCnts)],

    run_influence(Tracker, TagTopNN, Comm, Emo, init_params(top_n, AttrsNN, Params)).




%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the back-off timer.
% @end  --
init([Tracker, RunTag, RegComm, RegEmo, Params]) ->

    process_flag(trap_exit, true),

    % NOTE: The `datasets' and `toppers' are currently necessary for a successful Top-N run.
    [Period,
     Report,
     DataSets,
     Toppers] = pprops:get_values([{period, 7}, {report, false}, {datasets, #{}}, {toppers, #{}}], Params),

    % Our invoker may want to use a subset of the attributes
    InitAttrs = proplists:get_value(init_attrs, Params, all),
    {AllAttrs,
     InclAttrs,
     ExclAttrs} = incl_excl_attributes(InitAttrs),

    % TODO: The percent-biggies report needs to be converted to the 3-dataset method
    {Biggies,
     BigTag} = case proplists:get_value(method, Params, ?INIT_METHOD) of
        {biggies, Pct} -> {player:get_biggies(Tracker, Pct), ?str_fmt("p~B", [round(100*Pct)])};
        {top_n,   N}   -> {maps:get(N, Toppers),             ?str_fmt("n~B", [N])}
    end,

    % Log some feedback for this run
    BigInfo = fun({Comm, {P100, Cnt, Accts}}) ->
        ?str_FMT("{~s:~B%,tw=~B,cnt=~B}", [Comm, round(100 * P100), Cnt, length(Accts)])
    end,

    Name = ?bin_fmt("~s_~s_~s_~s_~s", [Tracker, RunTag, RegComm, RegEmo, BigTag]),
    ?info("Modelling '~s' influence: ds~p big~p", [Name,
                                                   [{S,maps:size(DS)} || {S,DS} <- maps:to_list(DataSets)],
                                                   [BigInfo(Grp) || Grp <- Biggies]]),

    % Prepare Weka modelling input.  We're going to need three datasets.
    DataSetter = fun(Step, Players) ->
        Relation = ?str_fmt("~s.~s", [Name, Step]),
        ?info("Creating ARFF: ~s", [Relation]),

        % The Big Players are the same with each.
        {ok, ARFF} = arff:from_biggies(Relation, RegComm, RegEmo, Biggies, Players, Period),
        list_to_binary(ARFF)
    end,
    ARFFs = maps:map(DataSetter, DataSets),

    % Function to name a working CSV file for Weka (use to fold from the ARFF map)
    ToCSV = fun(_, ARFF) ->
        Parts = string:split(ARFF, ".", trailing),
        CSV   = ?str_fmt("~s.csv", [hd(Parts)]),
        list_to_binary(CSV)
    end,
    CSVs = maps:map(ToCSV, ARFFs),

    % We should be good to go...!
    {ok, idle, #data{tracker    = Tracker,
                     name       = Name,
                     datasets   = ARFFs,
                     work_csvs  = CSVs,
                     attributes = AllAttrs,
                     init_attrs = InitAttrs,
                     incl_attrs = InclAttrs,
                     excl_attrs = ExclAttrs,
                     reg_comm   = RegComm,
                     reg_emo    = RegEmo,
                     period     = Period,
                     biggies    = Biggies,
                     jvm_node   = raven:get_jvm_node(Tracker),
                     models     = queue:new(),
                     report     = Report}}.



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
handle_event({call, _From}, get_outcome, Data = #data{name = Name}) ->
    ?debug("Waiting for outcome: ~s", [Name]),
    {keep_state, Data, [postpone]};


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
    {_,
     InclAttrs,
     ExclAttrs} = incl_excl_attributes(Data#data.init_attrs),

    {next_state, idle, Data#data{incl_attrs = InclAttrs,
                                 excl_attrs = ExclAttrs,
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
                                   datasets   = ARFFs,
                                   excl_attrs = ExclAttrs,
                                   work_csvs  = WorkCSVs,
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
    {say, JVM} ! {self(), WorkRef, regress, jsx:encode(#{datasets    => ARFFs,
                                                         exclude     => list_to_binary(ExclRE),
                                                         work_csvs   => WorkCSVs,
                                                         eval_method => ?EVAL_METHOD})},

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
    case Data#data.report of
        false -> ok;
        true  ->
            {RptStat,
             RptFPath} = report_all(Data),
            ?info("Reported ~s created: path[~s] stat[~p]", [Name, RptFPath, RptStat])
    end,
    keep_state_and_data;


done({call, From}, get_outcome, #data{name       = Name,
                                      incl_attrs = InclAttrs,
                                      results    = Results}) ->
    %
    CorrScore = maps:get(correlation, Results, 0.0),
    ?info("Model ~s outcome: score[~.4f] attrs~p", [Name, CorrScore, InclAttrs]),

    {keep_state_and_data, [{reply, From, {CorrScore, InclAttrs}}]};


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
% @doc  Initializes the independent attribute lists.
% @end  --
init_attributes() ->
    [arff:make_attribute(big, Code, Emo) || Code <- arff:get_big_comm_codes(),
                                            Emo  <- ?EMOTIONS].



%%--------------------------------------------------------------------
-spec incl_excl_attributes(Attrs :: [atom()] | all) -> {[atom()],
                                                        [atom()],
                                                        [atom()]}.
%%
% @doc  Initializes the include/exclude independent attribute lists
%       when the caller already knows what should be included.
% @end  --
incl_excl_attributes(Attrs) ->

    AllAttrs = init_attributes(),
    case Attrs of
        all   -> {AllAttrs, AllAttrs, ?EXCLUDED_ATTRS};
        Incls ->
            Excls = lists:filter(fun(A) -> not(lists:member(A, Incls)) end, AllAttrs),
            {AllAttrs, Incls, ?EXCLUDED_ATTRS ++ Excls}
    end.



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

    % Important: sort the keys so we always remove parameters in the same order
    lists:foreach(Caster, lists:sort(maps:keys(Coeffs))),

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
                  _           -> <<"Pct">>
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
                        Coeff    -> ?io_fmt(FOut, ",~.6f", [Coeff])
                    end,
                    Coeffs end,

    ?io_fmt(FOut, "~B,~s,~s,~s,~.6f,~B",
            [Line, Tracker, RegComm, RegEmo, CorrScore,InstCnt]),
    lists:foreach(Attribber, Attrs),
    ?io_fmt(FOut, ",~.6f~n", [Intercept]),

    % Only the line number changes in the accumulator
    {FOut, Line+1, Attrs, Data}.



%%--------------------------------------------------------------------
-spec extend_run_tag(RunTag   :: stringy(),
                     Comm     :: comm_code(),
                     Emo      :: emotion(),
                     MethRng  :: method_range()) -> string().
%%
% @doc  Pack all the information we can into a run tag.  Note that the
%       tracker gets prepended later in run_influence
% @end  --
extend_run_tag(RunTag, Comm, Emo, {Method, From, UpTo}) ->

    case Method of
        biggies -> ?str_FMT("~s_big~f-~f_~s_~s", [RunTag, 100*From, 100*UpTo, Comm, Emo]);
        top_n   -> ?str_FMT("~s_n~B-~B_~s_~s", [RunTag, From, UpTo, Comm, Emo])
    end.



%%--------------------------------------------------------------------
-spec run_influence(Tracker :: tracker(),
                    RunTag  :: stringy(),
                    Options :: proplist()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments
% @end  --
run_influence(Tracker, RunTag, Options) ->
    run_influence(Tracker, RunTag, [tter, oter, rter], ?EMOTIONS, Options).



%%--------------------------------------------------------------------
-spec run_influence(Tracker   :: tracker(),
                    RunTag    :: stringy(),
                    CommCodes :: comm_code()
                               | comm_codes(),
                    Emotions  :: emotion()
                               | emotions(),
                    Params    :: proplist()) -> proplist().
%%
% @doc  Run the Twitter/Emo influence experiments per the specified
%       parameters.  This function is the generalized work-horse for
%       the more user-friendly wrapper functions.
% @end  --
run_influence(Tracker, RunTag, CommCode, Emotions, Params) when is_atom(CommCode) ->
    run_influence(Tracker, RunTag, [CommCode], Emotions, Params);


run_influence(Tracker, RunTag, CommCodes, Emotion, Params) when is_atom(Emotion) ->
    run_influence(Tracker, RunTag, CommCodes, [Emotion], Params);


run_influence(Tracker, RunTag, CommCodes, Emotions, Params) ->

    % Each influence modeller handles an emo/comm pair for the regular players
    % NOTE: Only the `normal' single report sends multiple emotions and comms.
    Pairs = [{Emo, Comm} || Emo <- Emotions, Comm <- CommCodes],

    % In spite of the variable name, the `method' is not optional here:
    % Check for biggie|top_n range reports & setup a single emo/comm pair
    {Method,
     Inputs} = case proplists:get_value(method, Params) of

                    {biggies, Lo, Hi} ->
                        Mults = lists:seq(0, trunc((Hi - Lo) / ?INC_BIG_PCT)),
                        P100s = [?MIN_BIG_PCT + (M * ?INC_BIG_PCT) || M <- Mults],
                        {biggies,
                            lists:zip(P100s, lists:duplicate(length(P100s), hd(Pairs)))};

                    {top_n, Lo, Hi} ->
                        {next_n, lists:zip(lists:seq(Lo, Hi),
                                           lists:duplicate(Hi-Lo+1, hd(Pairs)))};

                    % Otherwise, we have a single report
                    _ ->
                        {normal, lists:zip(lists:seq(1, length(Pairs)), Pairs)}
    end,
    ?debug("Influence by ~s: ~p", [Method, Pairs]),

    Runner  = fun({Param, {RegEmo, RegComm}}) ->
                  % Top-N range options need to be prefixed with the current Top-N
                  RunOpts = case Method of
                                normal  -> Params;
                                biggies -> [{method, {biggies, Param}} | Params];
                                next_n  -> [{method, {top_n,   Param}} | Params]
                            end,

                  % The model startup may fail if we don't have full data for all comm codes
                  Model = case start_link(Tracker, RunTag, RegComm, RegEmo, RunOpts) of
                      {ok, Pid} -> influence:go(Pid), Pid;
                      ignore    -> none
                  end,
                  {Param, Model} end,

    Models  = lists:map(Runner, Inputs),

    % Create an overview report:
    % The index number is either a general model counter OR the N in a Top-N range report
    Report = ?str_fmt("~s_~s", [Tracker, RunTag]),
    {ok, FOut,
         FPath} = influence:report_open(Report, Params),
    Reporter = fun({Ndx, Model}) ->
                   % Note the FSM may block until it finishes modelling
                   influence:report_line(Model, FOut, Ndx),
                   Ndx+1 end,
    lists:foreach(Reporter, Models),

    % Close the report, collect attributes, and shutdown the modelling FSMs
    FStatus = file:close(FOut),
    ?info("Results: file[~s] stat[~p]", [FPath, FStatus]),

    Attribber = fun({Ndx, none})  -> {Ndx, []};
                   ({Ndx, Model}) ->
                    Attrs = influence:get_outcome(Model),
                    influence:stop(Model),
                    {Ndx, Attrs} end,
    lists:map(Attribber, Models).

