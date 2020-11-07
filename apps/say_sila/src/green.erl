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

-export([start/1,       start/2,
         start_link/1,  start_link/2,
         stop/0,
         make_arff/0,
         re_pattern/0,
         run_biggies/0,
         use_top_n/1, use_top_n/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-import(lists, [foldl/3]).
-import(proplists, [get_value/2]).

% Quickies for development
-export([go/0,   go/1,
         opts/0, opts/1]).

%%%-------------------------------------------------------------------
go() ->
    go(gw).


go(Tracker) ->
    start(Tracker, opts(biggies)).


%%%-------------------------------------------------------------------
opts() -> opts(q1).

opts(green)   -> [no_retweet, {start, {2019, 10, 1}}, {stop, {2020, 7, 1}}];
opts(q1)      -> [no_retweet, {start, {2020,  1, 1}}, {stop, {2020, 4, 1}}];
opts(jan)     -> [no_retweet, {start, {2020,  1, 1}}, {stop, {2020, 2, 1}}];
opts(day)     -> [no_retweet, {start, {2020,  1, 1}}, {stop, {2020, 1, 2}}];
opts(biggies) -> [no_retweet| biggies:period(train)].


-include("sila.hrl").
-include("ioo.hrl").
-include("player.hrl").
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
-spec start(Tracker :: tracker()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start(Tracker) ->
    start(Tracker, opts()).



%%--------------------------------------------------------------------
-spec start(Tracker :: tracker(),
            Options :: proplist()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start(Tracker, Options) ->
    start_up(start, Tracker, Options).



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
    start_up(start_link, Tracker, Options).



%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for modelling enviromentalism.
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).



%%--------------------------------------------------------------------
-spec make_arff() -> arff:arff_return()
                   | none.
%%
% @doc  Create an ARFF with the server's environmenal tweets.
% @end  --
make_arff() ->
    % This may take a while depending on the tweet count
    gen_server:call(?MODULE, make_arff, 60 * 1000).



%%--------------------------------------------------------------------
-spec re_pattern() -> re:mp().
%%
% @doc  Returns a compiled regular expression to be used to identify
%       texts talking about the environment.
% @end  --
re_pattern() ->
    {ok, RE} = re:compile(<<"environment"
                            "|[[:<:]]env[[:>:]]"        % WN3.1 "environmentalism"
                            "|[[:<:]]conserv"                   % sister term
                            "|[[:<:]]preserv"                   % direct hyperonym
                            "|[[:<:]]sav(e|es|ing)[[:>:]]">>,   % direct hyperonym
                          [caseless]),
    RE.



%%--------------------------------------------------------------------
-spec run_biggies() -> biggies:verifications().
%%
% @doc  Performs a set of "big players" tracking runs using tweets
%       with an environmental theme.
% @end  --
run_biggies() ->
    biggies:run_top_n(n, all, green_00, [{pattern,   re_pattern()},
                                         {data_mode, variation},
                                         {h0_tweets, 31},
                                         {sweep,     1}]).          % To start!



%%--------------------------------------------------------------------
-spec use_top_n(N :: pos_integer()) -> ok.

-spec use_top_n(N         :: pos_integer(),
                CommCodes :: comm_code()
                           | comm_codes()) -> ok.
%%
% @doc  Strips the server's tweets to use only the Top N users as
%       reported by the player server.  The caller may specifiy one
%       or more communication codes.  Only `oter' and `rter' are
%       currently supported, and `oter' is the default.
% @end  --
use_top_n(N) ->
    use_top_n(N, oter).


use_top_n(N, CommCodes) ->
    gen_server:cast(?MODULE, {use_top_n, N, CommCodes}).



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
handle_call(make_arff, _From, State = #state{tracker = Tracker,
                                             tweets  = Tweets,
                                             options = Options}) ->
    Response = case Tweets of
        [] -> none;
        _  -> arff:from_tweets(?str_fmt("tweets.~s.env", [Tracker]),
                               Tweets,
                               [{comment, ?str_fmt("~s: ~p", [?MODULE, Options])}])
    end,
    {reply, Response, State};


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
    JobOpts  = [{tag, environment},
                {pattern, re_pattern()},
                {timeout, JobCnt * ?TWITTER_DB_TIMEOUT} | RunOpts],

    % Get the workers started pulling and filtering tweets
    DoWork = fun (Job, Acc) ->
        Worker = daily_tweets:spawn_link(Tracker, Job, JobOpts),
        Acc#{Worker => Job}
    end,
    Workers = foldl(DoWork, #{}, Jobs),

    {noreply, State#state{options = Options,
                          workers = Workers}};


handle_cast({use_top_n, N, CommCodes}, State = #state{tracker = Tracker,
                                                      tweets  = Tweets}) ->
    TopN  = player:get_top_n(Tracker, N, CommCodes, [user_list]),
    Users = gb_sets:from_list(TopN),

    [?debug("Big Player: ~s", [U]) || U <- TopN],

    NewTweets = lists:filter(fun(#tweet{screen_name = U}) ->
                                gb_sets:is_member(string:casefold(U), Users)
                                end,
                             Tweets),
    {noreply, State#state{tweets = NewTweets}};


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


handle_info({daily_tweets, Worker, Tracker, DayTweets}, State = #state{tracker = Tracker,
                                                                       tweets  = Tweets,
                                                                       workers = Workers}) ->
    % Update rankings for Twitter user
    [player:tweet(Tracker, T) || T <- DayTweets],

    % Update state to reflect the work done
    JobOpts = maps:get(Worker, Workers),
    NewTweets = case DayTweets of
        [] ->
            ?warning("No ~s/environmental tweets on ~s: pid~p",
                       [Tracker, dts:str(get_value(start, JobOpts)), Worker]),
            Tweets;
        _ ->
            DayTweets ++ Tweets
    end,
    {noreply, State#state{tweets  = NewTweets,
                          workers = Workers#{Worker => [{status, complete} | JobOpts]}}};



handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec start_up(Starter :: start|start_link,
               Tracker :: tracker(),
               Options :: proplist()) -> gen:start_ret().
%%
% @doc  Startup function for modelling enviromentalism.
% @end  --
start_up(Starter, Tracker, Options) ->
    Args = [Tracker, Options],
    gen_server:Starter({?REG_DIST, ?MODULE}, ?MODULE, Args, []).



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
