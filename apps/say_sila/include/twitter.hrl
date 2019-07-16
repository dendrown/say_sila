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
                type            :: tweet | retweet | undefined,
                timestamp_ms    :: pos_integer(),
                emotions        :: rec_emos(),
                %---------------------------------- % User
                screen_name     :: binary(),
                name            :: binary(),
                description     :: binary(),
                text            :: binary() | ignored,
                %---------------------------------- % Retweeted author
                rt_id           :: binary(),
                rt_screen_name  :: binary() }).
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
