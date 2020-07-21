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
%% @copyright 2018-2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_player_included).
-define(_player_included, ack).

-include("emo.hrl").
-include("twitter.hrl").

-define(MIN_COMMS_COUNT,    3).                 % Minimum user activity for processing

-define(COMM_TYPES,     #{tter => tweeter,
                          oter => originator,
                          rter => retweeter,
                          rted => retweeted,
                          tmed => mentioned}).
-define(COMM4_CODES,    [         oter,       rter,      rted,      tmed]).
-define(COMM_CODES,     [tter,    oter,       rter,      rted,      tmed]).
-type comm_code()   ::   tter|    oter|       rter|      rted|      tmed.
-type comm_codes()  ::  [comm_code()].


%%--------------------------------------------------------------------
-type count_tree()  :: gb_trees:tree(integer(), list()).

-record(comm, {cnt  :: non_neg_integer(),   % FIXME: We already have a count in emos
              %msgs :: tweets(),
               emos :: emos() }).
-type comm()  :: #comm{}.
-type comms() :: [comm()].

-define(NEW_COMM,   #comm{cnt  = 0,
                         %msgs = [],
                          emos = emo:stoic(0) }).


%%--------------------------------------------------------------------
-record(profile, {comms = #{} :: map(),     % Comms totals for user
                  lots  = #{} :: map()}).   % map(key=day, val=map(comms))
%type profile() :: #profile{}.

-endif.
