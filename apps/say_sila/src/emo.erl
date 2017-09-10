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
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(emo).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([add/2]).

-include("twitter.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec add(Emo1 :: emotions(),
          Emo2 :: emotions()) -> emotions().
%
% @doc  Combines the two `emotions' records by adding the values for
%       each individual emotion.
% @end  --
add(Emo1, Emo2) ->
    #emotions{anger   = Emo1#emotions.anger   + Emo2#emotions.anger,
              fear    = Emo1#emotions.fear    + Emo2#emotions.fear,
              sadness = Emo1#emotions.sadness + Emo2#emotions.sadness,
              joy     = Emo1#emotions.joy     + Emo2#emotions.joy}.



%%====================================================================
%% Internal functions
%%====================================================================
