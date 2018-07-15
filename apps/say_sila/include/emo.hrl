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
-type emotion()  ::  anger| fear| sadness| joy.
-type emotions() :: [emotion()].


%%%-------------------------------------------------------------------
-record(emos, {count   = 0   :: integer(),      % Text source count
               levels  = #{} :: map() }).
-type emos()     :: #emos{}.
-type rec_emos() :: undefined | emos().

-define(emo_level(Emo, Rec),    maps:get(Emo, Rec#emos.levels)).

-endif.
