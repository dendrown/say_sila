%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Definitions for Twitter Tweets
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_twitter_included).
-define(_twitter_included, ack).

-include("dts.hrl").
-include("raven.hrl").

-define(TWITTER_DB_TIMEOUT, (10 * 60 * 1000)).      % FIXME: Rework the DB-pull logic
-record(tweet, {id              :: binary(),
                rt_id           :: binary(),        % rt_ means "retweet"
                rt_screen_name  :: binary(),
                timestamp_ms    :: pos_integer(),
                screen_name     :: binary(),
                text            :: ignored | binary(),
                type            :: tweet | retweet | undefined,
                emotions        :: rec_emotions() }).
-type tweet()  :: #tweet{}.
-type tweets() :: [tweet()].

%        This is the current Mnesia-based design (in process)
-record(tweet_lot, {dts      :: datetime(),
                    tweets   :: tweets() }).
-type tweet_lot() :: #tweet_lot{}.


-record(player, {screen_name :: binary(),
                 tweet_cnt   :: integer() }).
-type player()      :: #player{}.
-type players()     :: [player()].
-type big_players() :: [player()].


-endif.
