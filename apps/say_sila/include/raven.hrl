%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Definitions for emotional treatment of tweets
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_raven_included).
-define(_raven_included, ack).

-define(EMOTIONS,   [anger, fear, sadness, joy]).

-define(BEG_EPOCH,  {{1970,  1,  1}, { 0,  0,  0}}).
-define(END_EPOCH,  {{9999, 12, 31}, {23, 59, 59}}).


% @doc Emotions for analysis
-record(emotions, {count   = 0   :: integer(),
                   levels  = #{} :: map() }).
-type emotions() :: #emotions{}.



% @doc File handling for one emotion
-record(emo_file, {fpath_csv :: string(),
                   fpath_png :: string(),
                   io        :: pid() }).
-type emo_file() :: #emo_file{}.



% @doc Slots are for keeping everything about a player category in one place
-record(report, {category               :: atom(),
                 count    = 0           :: integer(),
                 beg_dts  = ?END_EPOCH  :: tuple(),
                 end_dts  = ?BEG_EPOCH  :: tuple(),
                 emotions = #{}         :: map() }).
-type report() :: #report{}.


-endif.
