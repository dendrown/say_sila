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

-define(EMOTIONS,       [anger, fear, sadness, joy]).
-define(MAX_TOP_HITS,   5).

-define(BEG_EPOCH,      {{1970,  1,  1}, { 0,  0,  0}}).
-define(END_EPOCH,      {{9999, 12, 31}, {23, 59, 59}}).


% Emotions for analysis
-record(emotions, {count   = 0   :: integer(),
                   levels  = #{} :: map() }).
-type emotions() :: #emotions{}.



% File handling for one emotion
-record(emo_file, {fpath_csv :: string(),
                   fpath_png :: string(),
                   io        :: pid() }).
-type emo_file() :: #emo_file{}.



% Report information on emotional analysis
%
% NOTE: `emotions' may be set to `info' if the record instance is informational only,
%       usually for (WUI) user feedback about the real report
-record(report, {category                  :: atom(),
                 num_players = 0           :: integer(),
                 num_tweets  = 0           :: integer(),
                 player_set                :: term(),           % Bookkeeping for subreports
                 beg_dts     = ?END_EPOCH  :: tuple(),
                 end_dts     = ?BEG_EPOCH  :: tuple(),
                 emotions    = #{}         :: map() | info,
                 top_hits    = #{}         :: map() }).         % emo => [level, tweet]
-type report()  :: #report{}.
-type reports() :: [{atom(), report()}].                        % proplist of reports


-endif.
