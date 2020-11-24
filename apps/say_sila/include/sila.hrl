%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Common definitions for Sila
%%
%% @copyright 2017-2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_sila_included).
-define(_sila_included, ack).

-define(REG_DIST, local).
-define(WORK_DIR, "/srv/say_sila").
-define(DETS_DIR, ?WORK_DIR "/dets").
-define(TMP_DIR,  "/tmp/say_sila").                 % Shared with fnodes

-define(DEFAULT_BIG_P100, 0.15).
-define(DEFAULT_PERIOD,   "day").

-define(TRACKERS, [cc,  gw]).
-type tracker() :: cc | gw | all.                   % TODO: Make sense of `all'

% Analysis with Weka
-type data_mode() :: level | variation.
-type eval_mode() :: cv | parms | test.
-type learner()   :: lreg | gproc.

% Database macros
-define(null_val_not(X, Null, NotNull), case (X) of null -> Null;       _ -> NotNull end).
-define(null_to_undef(X),               case (X) of null -> undefined;  _ -> X       end).


-endif.
