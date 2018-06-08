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

-include("emo.hrl").
-include("twitter.hrl").

-define(MIN_COMMS_COUNT,    3).                 % Minimum user activity for processing

-define(COMM_TYPES,     [tweeter, originater, retweeter, retweeted, mentioned]).
-define(COMM_CODES,     [tter,    oter,       rter,      rted,      tmed     ]).
-type comm_code()   ::   ttter|   oter|       rter|      rted|      tmed.
-type comm_codes()  ::  [comm_code()].


%%--------------------------------------------------------------------
-type count_tree()  :: gb_trees:tree(integer(), list()).

-record(comm, {cnt  :: non_neg_integer(),
               emos :: emotions(),
               msgs :: tweets() }).
-type comm()  :: #comm{}.
-type comms() :: [comm()].

-endif.
