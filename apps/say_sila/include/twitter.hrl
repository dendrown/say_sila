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

-record(emotions, {anger       :: float(),
                   fear        :: float(),
                   sadness     :: float(),
                   joy         :: float() }).
-type emotions() :: #emotions{}.


-record(tweet, {id          :: binary(),
                timestamp_ms:: pos_integer(),
                screen_name :: binary(),
                text        :: binary(),
               %-- deprecated emotion format --
                anger       :: float(),
                fear        :: float(),
                sadness     :: float(),
                joy         :: float() }).
-type tweet()  :: #tweet{}.
-type tweets() :: [tweet()].


-record(player, {screen_name :: binary(),
                 tweet_cnt   :: integer() }).
-type player()      :: #player{}.
-type players()     :: [player()].
-type big_players() :: [player()].


-endif.
