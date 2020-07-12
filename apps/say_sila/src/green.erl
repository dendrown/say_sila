%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila functionality for analysis of environmentalism.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(green).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/1, start_link/2,
         stop/0,
         re_pattern/0,
         report/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-import(lists, [foldl/3]).
-import(proplists, [get_value/2]).

% Quickies for development
-export([opts/0]).
opts() -> [no_retweet, {start, {2020, 1, 1}}, {stop, {2020, 4, 1}}].


-include("sila.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("llog/include/llog.hrl").


-record(state, {tracker       :: tracker(),
                tweets  = []  :: tweets(),
                options = []  :: proplist(),
                workers = #{} :: #{pid() => proplist()} }).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(Tracker :: tracker()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start_link(Tracker) ->
    start_link(Tracker, opts()).



%%--------------------------------------------------------------------
-spec start_link(Tracker :: tracker(),
                 Options :: proplist()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start_link(Tracker, Options) ->
    Args = [Tracker, Options],
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, Args, []).



%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for modelling enviromentalism.
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).



%%--------------------------------------------------------------------
-spec re_pattern() -> re:mp().
%%
% @doc  Returns a compiled regular expression to be used to identify
%       texts talking about the environment.
% @end  --
re_pattern() ->
    {ok, RE} = re:compile(<<"environment|[[:<:]]env[[:>:]]">>,
                          [caseless]),
    RE.


%%--------------------------------------------------------------------
-spec report(Tracker :: tracker(),
             Worker  :: pid(),
             Results :: {tweets, list()}) -> ok.
%%
% @doc  Reports results from workers collecting and filtering tweets.
% @end  --
report(Tracker, Worker, Results) ->
    gen_server:cast(?MODULE, {report, Tracker, Worker, Results}).



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
%% init:
%%
% @doc  Initialization for the Twitter access server.
% @end  --
init([Tracker, Options]) ->

    ?notice("Initializing analysis of enviromentalism"),
    process_flag(trap_exit, true),

    % Get environmental tweets per the specified options
    gen_server:cast(self(), {get_tweets, Options}),

    {ok, #state{tracker = Tracker}}.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _) ->
    Why.



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
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_cast
%%
% @doc  Process async messages
% @end  --
handle_cast({get_tweets, Options}, State = #state{tracker = Tracker}) ->
    {PeriodStart,
     PeriodStop,
     RunOpts} = daily:extract_period(Tracker, Options),

    % Create jobs for individual days in the period
     MakeWork = fun Recur(CurrDay, Acc = {Jobs, Cnt}) ->
        case daily:step(CurrDay, PeriodStop) of
            {NextDay,
             DayJob} -> Recur(NextDay, {[DayJob|Jobs], Cnt+1});
            stop     -> Acc
        end
    end,

    % The workers will all hit the twitter gen_server at about the same time.
    % As they will be servered one-at-a-time, use an appropriate timeout value.
    {Jobs,
     JobCnt} = MakeWork(PeriodStart, {[], 0}),
    JobOpts  = [{timeout, JobCnt * ?TWITTER_DB_TIMEOUT} | RunOpts],

    % Get the workers started pulling and filtering tweets
    EnvRE  = re_pattern(),
    DoWork = fun (Job, Acc) ->
        Worker = spawn_link(fun() -> get_tweets(Tracker, EnvRE, Job++JobOpts) end),
        Acc#{Worker => Job}
    end,
    Workers = foldl(DoWork, #{}, Jobs),

    {noreply, State#state{options = Options,
                          workers = Workers}};


handle_cast({report, Tracker, Worker, {tweets, Results}}, State = #state{tracker = Tracker,
                                                                         tweets  = Tweets,
                                                                         workers = Workers}) ->
    JobOpts = maps:get(Worker, Workers),
    NewTweets = case Results of
        [] ->
            ?warning("No ~s/environmental tweets on ~s: pid~p",
                       [Tracker, dts:str(get_value(start, JobOpts)), Worker]),
            Tweets;
        _ ->
            Results ++ Tweets
    end,
    {noreply, State#state{tweets  = NewTweets,
                          workers = Workers#{Worker => [{status, complete} | JobOpts]}}};



handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info({'EXIT', Worker, Why}, State = #state{workers = Workers}) ->

    Job = maps:get(Worker, Workers),
    NewWorkers = case Why of
        normal  ->
            %?debug("Worker ~p finished: job~p", [Worker, Job]),
            maps:remove(Worker, Workers);
        _ ->
            ?error("Worker ~p failed: why[~p] job~p", [Worker, Why, Job]),
            Workers#{Worker => [{fail, Why} | Job]}
    end,
    {noreply, State#state{workers = NewWorkers}};


handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec get_tweets(Tracker :: tracker(),
                 Pattern :: re:mp(),
                 Options :: proplist()) -> ok.
%%
% @doc  Retrieves and processes the tweets for a day.
% @end  --
get_tweets(Tracker, Pattern, Options) ->
    %?debug("Getting ~s tweets: ~p", [Tracker, Options]),
    DayTweets = twitter:get_tweets(Tracker, all, Options),
    EnvFilter = fun(T, {Envs, EnvCnt, DayCnt}) ->
        case re:run(T#tweet.text, Pattern) of
            nomatch -> {Envs,     EnvCnt,   DayCnt+1};
            _       -> {[T|Envs], EnvCnt+1, DayCnt+1}
        end
    end,
    {EnvTweets,
     EnvCnt,
     DayCnt} = foldl(EnvFilter, {[], 0, 0}, DayTweets),

    ?info("Environmental tweets for ~s: cnt[~3B of ~4B] pct[~.1f%]",
          [dts:str(dts:day(get_value(start, Options))),
           EnvCnt, DayCnt,
           100.0 * (EnvCnt/DayCnt)]),
    green:report(Tracker, self(), {tweets, EnvTweets}).



%%====================================================================
%% Unit tests
%%--------------------------------------------------------------------
re_pattern_test() ->

    RE = re_pattern(),
    {match,_} = re:run(<<"This is env.">>, RE),
    {match,_} = re:run(<<"This is env\nBang!">>, RE),
    {match,_} = re:run(<<"This is the environment.">>, RE),
    {match,_} = re:run(<<"This is the #environment.">>, RE),

    nomatch = re:run(<<"This is enviro.">>, RE),
    nomatch = re:run(<<"This is the envonment.">>, RE).
