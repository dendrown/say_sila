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

-export([add/2,
         average/1,
         relevel/1,
         stoic/0]).

-include("raven.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec add(Emos1 :: emotions(),
          Emos2 :: emotions()) -> emotions().
%
% @doc  Combines the two `emotions' records by adding the values for
%       each individual emotion.
% @end  --
add(Emos1, Emos2) ->
    #emotions{count  = Emos1#emotions.count
                     + Emos2#emotions.count,
              levels = relevel(fun(Emo) -> {Emo,  maps:get(Emo, Emos1#emotions.levels)
                                                + maps:get(Emo, Emos2#emotions.levels)} end)}.



%%--------------------------------------------------------------------
-spec average(Emos :: emotions()) -> emotions().
%
% @doc  Returns an `emotions' record representing the average emotion
%       levels per tweet.
% @end  --
average(Emos = #emotions{count = Cnt}) ->
    if  0 =:= Cnt -> #emotions{};
        1 =:= Cnt -> Emos;
        Cnt > 1 ->
            #emotions{count  = 1,
                      levels = relevel(fun(Emo) -> {Emo, maps:get(Emo, Emos#emotions.levels) / Cnt} end)}
    end.



%%--------------------------------------------------------------------
-spec relevel(Closure :: fun((atom()) -> {atom(), float()})) -> map().
%
% @doc  Applies the Closure function to all emotion values in the
%       (wrapped) input map.
% @end  --
relevel(Closure) ->
    Levels = lists:map(Closure, ?EMOTIONS),
    maps:from_list(Levels).



%%--------------------------------------------------------------------
-spec stoic() -> map().
%
% @doc  Returns an `emotion' record representing "no emotion".
% @end  --
stoic() ->
    #emotions{count   = 1,
              levels  = #{anger   => 0.0,
                          fear    => 0.0,
                          sadness => 0.0,
                          joy     => 0.0}}.



%%====================================================================
%% Internal functions
%%====================================================================
