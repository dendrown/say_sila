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

-export([one/0, two/0, full/0, q1/0, q2/0, q4/0, q4q1/0, today/0,
         influence/0, influence/2, influence/3,
         influence_n/3]).

-include("sila.hrl").
-include("emo.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("types.hrl").
-include("twitter.hrl").

-define(twitter(Acct),  io_lib:format("[~s](https://twitter.com/~s)", [Acct, Acct])).

-define(SN,   <<"SCREEN NAME">>).
-define(OPTS, [{start, {2017, 10, 1}},
               {stop,  {2017, 11, 1}}]).



%%--------------------------------------------------------------------
%% Period shortcuts
%%--------------------------------------------------------------------
one()  -> [{start, {2017, 12, 31}}, {stop, {2018, 1, 1}}].
two()  -> [{start, {2017, 12, 30}}, {stop, {2018, 1, 1}}].

full() -> [{start, {2017, 10,  1}}, {stop, {2018, 7, 1}}].
q4()   -> [{start, {2017, 10,  1}}, {stop, {2018, 1, 1}}].   % Gonna need the year
q1()   -> [{start, {2018, 01,  1}}, {stop, {2018, 4, 1}}].
q2()   -> [{start, {2018, 04,  1}}, {stop, {2018, 7, 1}}].
q4q1() -> [{start, {2017, 10,  1}}, {stop, {2018, 4, 1}}].

today() ->
    Today = dts:dayize(calendar:local_time()),
    [{start, Today}, {stop, dts:add(Today, 1, day)}].
%%--------------------------------------------------------------------



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
                                {period, 7}]).



%%--------------------------------------------------------------------
-spec influence(Tracker :: tracker(),
                RunTag  :: stringy(),
                Options :: proplist()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments
% @end  --
influence(Tracker, RunTag, Options) ->
    % Start up a modelling FSM for all regular emo/comm combos
    % (The usual hierarchy is comm/emo, but we're switching around
    % for an easier-to-analyze report.)
    Report  = ?str_fmt("~s_~s", [Tracker, RunTag]),
    Runner  = fun(RegEmo, RegComm) ->
                  {ok,
                   Pid} = influence:start_link(Tracker, RunTag, RegComm, RegEmo, Options),
                  influence:go(Pid),
                  Pid end,
    Models  = [Runner(Emo, Comm) || Emo <- ?EMOTIONS, Comm <- [tter, oter, rter]],

    % Create an overview report
    {ok, FOut, FPath} = influence:report_open(Report),
    Reporter = fun(Model, Line) ->
                   % Note the FSM may block until it finishes modelling
                   influence:report_line(Model, FOut, Line),
                   Line+1 end,

    lists:foldl(Reporter, 0, Models),

    % Close the report, and shutdown the modelling FSMs
    FStatus = file:close(FOut),
    lists:foreach(fun(M) -> influence:stop(M) end, Models),
    {FStatus, FPath}.



%%--------------------------------------------------------------------
-spec influence_n(Tracker :: tracker(),
                  RunTag  :: stringy(),
                  N       :: non_neg_integer()) -> term().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts in each
%       category.  This function uses weekly periods.
% @end  --
influence_n(Tracker, RunTag, N) ->
    influence(Tracker, RunTag, [{method, {top_n, N}},
                                {period, 7}]).
