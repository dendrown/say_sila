%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Emotion Utilities
%%
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_emo_included).
-define(_emo_included, ack).

-define(EMOTIONS,   [anger, fear, sadness, joy]).


%%%-------------------------------------------------------------------
-record(emotions, {count   = 0   :: integer(),
                   levels  = #{} :: map() }).
-type emotions()     :: #emotions{}.
-type rec_emotions() :: undefined | #emotions{}.

-define(emo_level(Emo, Rec),    maps:get(Emo, Rec#emotions.levels)).

-endif.
