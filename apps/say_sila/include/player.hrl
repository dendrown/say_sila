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


%%--------------------------------------------------------------------
-type count_tree()  :: gb_trees:tree(integer(), list()).

-record(counts, {tt = 0 :: non_neg_integer() | count_tree(),    % Num original tweets
                 rt = 0 :: non_neg_integer() | count_tree(),    % Num times retweeted
                 tm = 0 :: non_neg_integer() | count_tree()}).  % Num times mentioned
-type counts() :: #counts{}.

-endif.
