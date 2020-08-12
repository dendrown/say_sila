%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila tweet-puller for a day's worth of tweets.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(daily_tweets).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([spawn_link/2, spawn_link/3,
         get_tweets/2, get_tweets/3]).


-import(lists, [foldl/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("sila.hrl").
-include("dts.hrl").
-include("types.hrl").
-include("twitter.hrl").
-include_lib("llog/include/llog.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec spawn_link(Tracker :: tracker(),
                 Options :: proplist()) -> pid()
                                          | stop.

-spec spawn_link(Tracker  :: tracker(),
                 StepOpts :: proplist(),
                 RunOpts  :: proplist()) -> pid()
                                          | stop.
%%
% @doc  Pulls a day of tweets with optional an optional filter.
%       Note that `StepOpts' are the options returned from a call
%       to `daily:step/2-3'.  The caller may keep them separate
%       or combine them with the run options.
% @end  --
spawn_link(Tracker, Options) ->
    Invoker = self(),
    spawn_link(fun() -> get_tweets(Invoker, Tracker, all, Options) end).


spawn_link(_, stop, _) ->
    stop;


spawn_link(Tracker, StepOpts, RunOpts) ->
    daily_tweets:spawn_link(Tracker, StepOpts ++ RunOpts).



%%--------------------------------------------------------------------
-spec get_tweets(Tracker :: tracker(),
                 Options :: proplist()) -> tweets().

-spec get_tweets(Tracker  :: tracker(),
                 Accounts :: twitter:account()
                           | twitter:accounts(),
                 Options  :: proplist()) -> tweets().
%%
% @doc  Retrieves and processes Twitter status messages for a day.
% @end  --
get_tweets(Tracker, Options) ->
    get_tweets(Tracker, all, Options).


get_tweets(Tracker, Accounts, Options) ->
    %?debug("Getting ~s tweets: ~p", [Tracker, Options]),
    DayTweets = twitter:get_tweets(Tracker, Accounts, Options),

    % Do they have a regular expression for filtering?
    case get_value(pattern, Options) of
        undefined -> DayTweets;
        Pattern ->
            % Find the tweets matching the pattern
            Filter = fun(T, {FltTweets, FltCnt, DayCnt}) ->
                case re:run(T#tweet.full_text, Pattern) of
                    nomatch -> {FltTweets,     FltCnt,   DayCnt+1};
                    _       -> {[T|FltTweets], FltCnt+1, DayCnt+1}
                end
            end,
            {FltTweets,
             FltCnt,
             DayCnt} = foldl(Filter, {[], 0, 0}, DayTweets),

            ?info("Tweets (~s) for ~s: cnt[~4B of ~5B] pct[~4.1f%]",
                  [get_value(tag, Options, filtered),
                   dts:str(dts:day(get_value(start, Options))),
                   FltCnt, DayCnt,
                   100.0 * (FltCnt/DayCnt)]),
            FltTweets
    end.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec get_tweets(Invoker  :: entity(),
                 Tracker  :: tracker(),
                 Accounts :: twitter:account()
                           | twitter:accounts(),
                 Options  :: proplist()) -> {daily_tweets,
                                             entity(),
                                             tracker(),
                                             tweets()}.
%%
% @doc  Retrieves and processes Twitter status messages for a day.
% @end  --
get_tweets(Invoker, Tracker, Accounts, Options) ->

    Invoker ! {daily_tweets, self(), Tracker, get_tweets(Tracker, Accounts, Options)}.

