%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc The "Say Sila" Twitter player (account) handler
%%
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_player_included).
-define(_player_included, ack).

-define(MIN_COMMS_COUNT,    3).                 % Minimum user activity for processing

-define(COMMS_TYPES,        [tweet, original, retweet, mention]).
-define(COMMS_CODES,        [tt,    ot,       rt,      tm     ]).


%%--------------------------------------------------------------------
-type count_tree()  :: gb_trees:tree(integer(), list()).

-record(counts, {tt = 0 :: non_neg_integer() | count_tree(),    % Tweets sent from a user
                 ot = 0 :: non_neg_integer() | count_tree(),    % Original tweets sent (not retweets)
                 rt = 0 :: non_neg_integer() | count_tree(),    % Times retweeted be another user
                 tm = 0 :: non_neg_integer() | count_tree()}).  % Times mentioned by another user
-type counts() :: #counts{}.

-define(prop_counts(Fld), {Fld, #counts.Fld}).


%%--------------------------------------------------------------------
-record(profile, {cnts :: counts(),
                  emos :: emotions() }).
-type profile() :: #profile{}.

-define(profile_cnts(Rec, Fld), Rec#profile.cnts#counts.Fld).
-define(profile_emos(Rec, Fld), Rec#profile.emos#emotions.Fld).


-endif.
