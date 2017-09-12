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


-define(BEG_EPOCH,  {{1970,  1,  1}, { 0,  0,  0}}).
-define(END_EPOCH,  {{9999, 12, 31}, {23, 59, 59}}).

% Emotions for analysis
-record(emotions, {anger   = 0.0 :: float(),
                   fear    = 0.0 :: float(),
                   sadness = 0.0 :: float(),
                   joy     = 0.0 :: float() }).
-type emotions() :: #emotions{}.


% @doc File handling for one emotion
-record(emo_file, {fpath :: string(),
                   io   :: pid() }).
-type emo_file() :: #emo_file{}.


% @doc Files for emotions MUST match the generic #emotions record
-record(emo_files, {anger       :: emo_file(),
                    fear        :: emo_file(),
                    sadness     :: emo_file(),
                    joy         :: emo_file() }).
-type emo_files() :: #emo_files{}.


% @doc Slots are for keeping everything about a player category in one place
-record(report, {category               :: atom(),
                 count    = 0           :: integer(),
                 beg_dts  = ?END_EPOCH  :: tuple(),
                 end_dts  = ?BEG_EPOCH  :: tuple(),
                 emotions = #{}         :: map() }).
-type report() :: #report{}.


-endif.
