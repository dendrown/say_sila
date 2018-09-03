%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc A try-it-out playpen for looking at data in "Say Sila".
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(adhoc).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([one/0, two/0, year/0, full/0, q1/0, q2/0, q4/0, q4q1/0, today/0,
         influence/0,       influence/2,    influence/3,
         influence_n/3,     influence_n/4,
         influence_nn/3,    influence_nn/4]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include("twitter.hrl").
-include_lib("llog/include/llog.hrl").

-define(twitter(Acct),  io_lib:format("[~s](https://twitter.com/~s)", [Acct, Acct])).

-define(SN,   <<"SCREEN NAME">>).
-define(OPTS, [{start, {2017, 10, 1}},
               {stop,  {2017, 11, 1}}]).


%%--------------------------------------------------------------------
% Influence reports
%
% TODO: These are firming up to the point we should probably move the
%       functionality over to the `influence' module.
-define(MIN_TOP_N,      5).
-define(MAX_TOP_N,      25).
-define(MIN_NN_SCORE,   0.6).



%%--------------------------------------------------------------------
%% Period shortcuts
%%--------------------------------------------------------------------
one()  -> [{start, {2017, 12, 31}}, {stop, {2018, 1, 1}}].
two()  -> [{start, {2017, 12, 30}}, {stop, {2018, 1, 1}}].

year() -> [{start, {2017, 08,  1}}, {stop, {2018, 8, 1}}].
full() -> [{start, {2017, 10,  1}}, {stop, {2018, 7, 1}}].
q4()   -> [{start, {2017, 10,  1}}, {stop, {2018, 1, 1}}].   % Gonna need the year
q1()   -> [{start, {2018, 01,  1}}, {stop, {2018, 4, 1}}].
q2()   -> [{start, {2018, 04,  1}}, {stop, {2018, 7, 1}}].
q4q1() -> [{start, {2017, 10,  1}}, {stop, {2018, 4, 1}}].

today() ->
    Today = dts:dayize(calendar:local_time()),
    [{start, Today}, {stop, dts:add(Today, 1, day)}].



%%--------------------------------------------------------------------
-spec influence() -> ok.
%%
% @doc  Do the Twitter/Emo influence experiments
% @end  --
influence() ->
    Tag = sila,
    lists:foreach(fun(Trk) -> influence(Trk, Tag) end, ?TRACKERS).



%%--------------------------------------------------------------------
-spec influence(Tracker :: tracker(),
                RunTag  :: stringy()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments.  The caller must
%       specify the tracker, but we default to biggie velocity at
%       1% over weekly periods.
% @end  --
influence(Tracker, RunTag) ->
    influence(Tracker, RunTag, [{method, {biggies, 0.01}},
                                {period, 7},
                                report]).



%%--------------------------------------------------------------------
-spec influence(Tracker :: tracker(),
                RunTag  :: stringy(),
                Options :: proplist()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments
% @end  --
influence(Tracker, RunTag, Options) ->
    run_influence(Tracker, RunTag, [tter, oter, rter], ?EMOTIONS, Options).



%%--------------------------------------------------------------------
-spec influence_n(Tracker :: tracker(),
                  RunTag  :: stringy(),
                  Emo_N   :: emotion()
                           | non_neg_integer()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts in each
%       category.  This function uses weekly periods.
% @end  --
influence_n(Tracker, RunTag, Emo) when is_atom(Emo) ->
    Comms  = [tter, oter, rter],
    Runner = fun(Comm) ->
                 Tag = ?str_fmt("~s_~s_~s", [RunTag, Comm, Emo]),
                 influence_n(Tracker, Tag, Emo, Comm)
                 end,
    lists:map(Runner, Comms);


influence_n(Tracker, RunTag, N) when is_integer(N) ->
    Options = [{method, {top_n, N}},
               {period, 7}],
    influence(Tracker, RunTag, Options).



%%--------------------------------------------------------------------
-spec influence_n(Tracker :: tracker(),
                  RunTag  :: stringy(),
                  Comm    :: comm_code(),
                  Emo     :: emotion()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This function uses weekly periods.
% @end  --
influence_n(Tracker, RunTag, Comm, Emo) ->
    influence_n(Tracker, RunTag, Comm, Emo, []).



%%--------------------------------------------------------------------
-spec influence_n(Tracker  :: tracker(),
                  RunTag   :: stringy(),
                  Comm     :: comm_code(),
                  Emo      :: emotion(),
                  IndAttrs :: [atom()]) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.  This
%       version of `influence_n' accepts an override for the initial
%       independent attributes.
%
%       This function uses weekly periods.
% @end  --
influence_n(Tracker, RunTag, Comm, Emo, IndAttrs) ->
    FromN   = ?MIN_TOP_N,
    UpToN   = ?MAX_TOP_N,
    TopOpts = [{method, {top_n, FromN, UpToN}},
               {period, 7}],

    Options = case IndAttrs of
        []   -> TopOpts;
        Inds -> [{init_attrs, Inds} | TopOpts]
    end,
    run_influence(Tracker, RunTag, [Comm], [Emo], Options).



%%--------------------------------------------------------------------
-spec influence_nn(Tracker :: tracker(),
                   RunTag  :: stringy(),
                   Emo     :: emotion()) -> ok.
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This `nn' function goes through the Top-N twice, using only
%       the emo/comm attributes from the higher ranking models.
%
%       This function runs `tter', `oter' and `rter' communications
%       using weekly periods.
% @end  --
influence_nn(Tracker, RunTag, Emo) ->
    lists:foreach(fun(C) -> influence_nn(Tracker, RunTag, C, Emo) end,
                  [tter, oter, rter]).



%%--------------------------------------------------------------------
-spec influence_nn(Tracker :: tracker(),
                   RunTag  :: stringy(),
                   Comm    :: comm_code(),
                   Emo     :: emotion()) -> proplist().
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
influence_nn(Tracker, RunTag, Comm, Emo) ->

    % Extend the run tag to capture all important information.
    % Note that the tracker gets prepended later in run_influence
    TagTopN  = ?str_FMT("~s_n~B-~B_~s_~s",
                        [RunTag, ?MIN_TOP_N, ?MAX_TOP_N, Comm, Emo]),

    % Do An initial run, keeping the results above a minimum correlation score
    MinScore = ?MIN_NN_SCORE,
    BaseRun  = lists:filter(fun({_, {Score, _}}) ->
                                abs(Score) >= MinScore
                                end,
                            influence_n(Tracker, TagTopN, Comm, Emo)),

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

    influence_n(Tracker, TagTopNN, Comm, Emo, AttrsNN).



%%--------------------------------------------------------------------
-spec run_influence(Tracker   :: tracker(),
                    RunTag    :: stringy(),
                    CommCodes :: comm_codes(),
                    Emotions  :: emotions(),
                    Options   :: proplist()) -> proplist().
%%
% @doc  Run the Twitter/Emo influence experiments per the specified
%       parameters.  This function is the generalized work-horse for
%       the more user-friendly wrapper functions.
% @end  --
run_influence(Tracker, RunTag, CommCodes, Emotions, Options) ->

    % Each influence modeller handles an emo/comm pair for the regular players
    Pairs   = [{Emo, Comm} || Emo <- Emotions, Comm <- CommCodes],
    PairCnt = length(Pairs),

    % In spite of the variable name, the `method' is not optional here:
    {Method,
     Inputs} = case proplists:get_value(method, Options) of

                   % The Top-N range report assumes a single emo/comm pair
                   {top_n, Lo, Hi} ->
                       {next_n, lists:zip(lists:seq(Lo, Hi),
                                          lists:duplicate(Hi-Lo+1, hd(Pairs)))};

                   % Otherwise, we have a biggie or single-Top-N report
                   _ ->
                       {normal, lists:zip(lists:seq(1, PairCnt), Pairs)}
               end,
    ?debug("Influence by ~s: ~p", [Method, Pairs]),

    Runner  = fun({N, {RegEmo, RegComm}}) ->
                  % Top-N range options need to be prefixed with the current Top-N
                  RunOpts = case Method of
                                normal -> Options;
                                next_n -> [{method, {top_n, N}} | Options]
                            end,
                  {ok, Pid} = influence:start_link(Tracker, RunTag, RegComm, RegEmo, RunOpts),
                  influence:go(Pid),
                  {N, Pid} end,

    Models  = lists:map(Runner, Inputs),

    % Create an overview report:
    % The index number is either a general model counter OR the N in a Top-N range report
    Report = ?str_fmt("~s_~s", [Tracker, RunTag]),
    {ok, FOut,
         FPath} = influence:report_open(Report, Options),
    Reporter = fun({Ndx, Model}) ->
                   % Note the FSM may block until it finishes modelling
                   influence:report_line(Model, FOut, Ndx),
                   Ndx+1 end,
    lists:foreach(Reporter, Models),

    % Close the report, collect attributes, and shutdown the modelling FSMs
    FStatus = file:close(FOut),
    ?info("Results: file[~s] stat[~p]", [FPath, FStatus]),

    Attribber = fun({Ndx, Model}) ->
                    Attrs = influence:get_outcome(Model),
                    influence:stop(Model),
                    {Ndx, Attrs} end,
    lists:map(Attribber, Models).

