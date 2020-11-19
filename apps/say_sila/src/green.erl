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
         clear_cache/0,
         get_stance/1,  get_stance/2,
         get_stances/0, get_stances/1,
         load_stances/1,
         make_arff/0,
         re_pattern/0,
         run_biggies/0,
         set_stance/2,
         use_top_n/1,   use_top_n/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-import(lists, [filter/2, foldl/3]).
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
opts() -> opts(day).                    % TODO: Find mystery period (q1?)

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
-include_lib("stdlib/include/ms_transform.hrl").

-define(STANCE_CACHE,   ?DETS_DIR "/green_stance").

-record(state, {tracker       :: tracker(),
                deniers = []  :: [binary()],        % Base climate denier accounts
                greens  = []  :: [binary()],        % Base green user accounts
                tweets  = []  :: tweets(),
                options = []  :: proplist(),
                workers = #{} :: #{pid() => proplist()} }).
-type state() :: #state{}.

-type stance()  :: green|denier|undefined.
-type stances() :: [stance()].


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
    gen_server:call(?MODULE, stop),
    dets:close(?STANCE_CACHE).



%%--------------------------------------------------------------------
-spec clear_cache() -> ok
                     | {error, term()}.
%%
% @doc  Clears the green-related DETS cache.
% @end  --
clear_cache() ->
    dets:delete_all_objects(?STANCE_CACHE).



%%--------------------------------------------------------------------
-spec get_stance(Account :: stringy()) -> {ok|twitter,
                                           stance()}.

-spec get_stance(Account :: stringy(),
                 Options :: options()) -> {ok|twitter,
                                           stance()}.
%%
% @doc  Queries the twitter server to determine if the stance of the
%       user represented by the specified Account is `green`, `denier'
%       or `undefined' if the stance cannot be determined.
%
%       Specifying the option `requery' will ignore any existing
%       value in the cache and force a call to the Twitter API.
% @end  --
get_stance(Account) ->
    get_stance(Account, []).


get_stance(Account, Options) ->
    gen_server:call(?MODULE, {get_stance, Account, Options}).



%%--------------------------------------------------------------------
-spec get_stances() -> #{binary() := stances()}.

-spec get_stances(Options :: uqam|options()) -> #{binary() := stances()}.
%%
% @doc  Creates a mapping of stances (`green' or `denier') to lists
%       of users.
%
%       The following options are available:
%       - format    : Return the mapping as `json' or and Erlang `map' (default)
%       - fpath     : Also save the mapping to the specified file path
% @end  --
get_stances() ->
    get_stances([]).


get_stances(uqam) ->
    green:get_stances([{format,json},
                       {fpath, "/tmp/sila-stances.json"}]);


get_stances(Options) ->
    % Cache queries are created at compile time!!
    Queries = #{denier => ets:fun2ms(fun ({Acct, denier}) -> Acct end),
                green  => ets:fun2ms(fun ({Acct, green})  -> Acct end)},

    Stances = maps:map(fun(_,Q) -> dets:select(?STANCE_CACHE, Q) end, Queries),

    % How do they want the results?
    Format = pprops:get_value(format, Options, map),
    Return = case Format of
        map  -> Stances;
        json -> jsx:encode(Stances)
    end,

    % Save a copy for development use?
    case pprops:get_value(fpath, Options) of
        undefined -> ok;

        FPath ->
            % The write method differs slightly for JSON and erlang terms
            ?info("Saving stances to ~s", [FPath]),
            file:write_file(FPath,
                            case Format of
                                json -> Return;
                                _    -> term_to_binary(Return)
                            end)
    end,
    Return.



%%--------------------------------------------------------------------
-spec load_stances(FPath :: stringy()) -> #{binary() := stance()}.
%%
% @doc  Load accounts from the specified file and return a map
%       keyed with those accounts, containing the stances of those
%       users.
% @end  --
load_stances(FPath) ->
    Accounts = load_screen_names(FPath),
    Throttle = gen_server:call(?MODULE, get_throttle),

    % TODO: create a more sophisticated throttling abstraction
    GetStance = fun(Acct, {Wait, Acc}) ->
        case Wait of
            ok      -> ok;
            twitter -> timer:sleep(Throttle)
        end,
        {NewWait,
         Stance} = get_stance(Acct),
        ?info("~s: ~s", [Acct, Stance]),
        {NewWait, Acc#{Acct => Stance}}
    end,
    {_,
     Stances} = foldl(GetStance, {ok, #{}}, Accounts),
    Stances.



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
-spec set_stance(Account :: stringy(),
                 Stance  :: stance()) -> ok | {error, term()}.
%%
% @doc  Allows a user-override to set green|denier stance for a given
%       user.
% @end  --
set_stance(Account, Stance) ->
    case check_stance(Account) of
        Stance ->
            ?info("User ~s stance is already ~s", [Account, Stance]);
        {_, OldStance} ->
            ?info("Setting user ~s stance: ~s -> ~s", [Account, OldStance, Stance]),
            cache_stance(Account, Stance)
    end.



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

    % Load the list of base accounts for deniers and green-minded folk
    GetBase = fun(Who) ->
        FPath = ?str_fmt("~s/resources/accounts/~s.lst", [code:priv_dir(say_sila), Who]),
        ?debug("Reading base ~s: ~s", [Who, FPath]),
        load_screen_names(FPath)
    end,

    % Get environmental tweets per the specified options
    gen_server:cast(self(), {get_tweets, Options}),

    {ok, #state{tracker  = Tracker,
                 deniers = GetBase(deniers),
                 greens  = GetBase(greens)}}.



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
handle_call({get_stance, Account, Options}, _From, State) ->

    Reply = case check_stance(Account, Options) of
        none ->
            Stance = query_stance(Account, State),
            cache_stance(Account, Stance),
            {twitter, Stance};

        {_, Stance} -> {ok, Stance}
    end,
    {reply, Reply, State};


handle_call(get_throttle, _From, State = #state{deniers = Deniers,
                                                greens  = Greens}) ->
    % Throttle requests so we don't exceed 180 every 15 minutes (720 req/hr or 5 sec/req)
    % TODO: (1) Abstract and formalize throttling (modules: green, pan).
    %       (2) Keep submitting requests until we near limit, then throttle
    Millis = 5250 * (length(Deniers) + length(Greens)),
    {reply, Millis, State};


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
    % Twitter limits our query rate severely. Cache what we know!
    ioo:make_fpath(?STANCE_CACHE),
    dets:open_file(?STANCE_CACHE, [{repair, true}, {auto_save, 60000}]),

    Args = [Tracker, Options],
    gen_server:Starter({?REG_DIST, ?MODULE}, ?MODULE, Args, []).



%%--------------------------------------------------------------------
-spec cache_stance(Account :: stringy(),
                   Stance  :: stance()) -> ok | {error, term()}.
%%
% @doc  Updates the DETS cache explicitly with the specified
%       key (Account) and value (Stance).
% @end  --
cache_stance(Account, Stance) ->
    dets:insert(?STANCE_CACHE, {types:to_binary(Account), Stance}).



%%--------------------------------------------------------------------
-spec check_stance(Account :: stringy()) -> none
                                          | {binary(), stance()}.

-spec check_stance(Account :: stringy(),
                   Options :: options()) -> none
                                          | {binary(), stance()}.
%%
% @doc  Checks the DETS cache for the specified key (Account) and
%       returns a key-value pair {Account, Stance}, or `none' if
%       the stance cache does not contain the specified key.
%
%       Specifying the only supported option [`requery'] will
%       cause the function to skip the cache check and return
%       `none' in all cases.
% @end  --
check_stance(Account) ->
    check_stance(Account, []).


check_stance(Account, Options) ->
    case pprops:get_value(requery, Options) of
        % Normal operation searches the cache
        undefined ->
            case dets:lookup(?STANCE_CACHE, types:to_binary(Account)) of
                []    -> none;
                [Hit] -> Hit
            end;

        % A requery assumes no-hit so the caller will query the service
        _ -> none
    end.



%%--------------------------------------------------------------------
-spec load_screen_names(FPath :: stringy()) -> [binary()].
%%
% @doc  Returns a list of the screen names contained (one per line)
%       in the specified file.
% @end  --
load_screen_names(FPath) ->

    ThrowOut = fun
        (<<$%,_/binary>>) -> true;                  % Comment %
        (X)               -> string:is_empty(X)     % Blank line
    end,

    case file:read_file(FPath) of
        {ok, Data} ->
            fp:remove(ThrowOut,
                      [string:trim(A) || A <- string:split(Data, <<"\n">>, all)]);

        {error, Why} ->
            ?error("Cannot load accounts: ~p", [Why]),
            []
    end.



%%--------------------------------------------------------------------
-spec query_stance(Account :: stringy(),
                   State   :: state()) -> stance().
%%
% @doc  Pulls the specified user's stance from Twitter.
% @end  --
query_stance(Account, #state{deniers = Deniers,
                             greens  = Greens}) ->
    CountFollowers = fun
        Recur([], Acc) -> Acc;

        Recur([B|RestBase], Acc) ->
            case twitter:is_following(Account, B) of
                undefined -> Acc;                       % Issue w/ account|query
                false     -> Recur(RestBase, Acc);
                true      -> Recur(RestBase, Acc+1)
            end
    end,

    % Make a decision when it's clear.  When it's not, Twitter will log messages.
    case [CountFollowers(Base, 0) || Base <- [Deniers, Greens]] of
        [0, 0] -> undefined;
        [_, 0] -> denier;
        [0, _] -> green;
        [D, G] ->
            ?warning("User ~s is following both sides: denier[~B] green[~B]",
                     [Account, D, G]),
            undefined
    end.



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
