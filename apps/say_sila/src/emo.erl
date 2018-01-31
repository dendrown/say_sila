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
         do_top_hits/2,
         is_stoic/1,
         relevel/1,
         stoic/0]).

-include("raven.hrl").
-include("twitter.hrl").
-include_lib("llog/include/llog.hrl").

-define(EPSILON, 0.00000001).


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
-spec do_top_hits(Tweet   :: tweet(),
                  TopHits :: map()) -> map().
%
% @doc  Determines if the specified Tweet is a "top hit" (if it registers
%       extremely high for one [or more] emotions), and if so, updates
%       the given TopHit map to include the Tweet, possibly removing a
%       previous high-emotion tweet with a lower emotion level.
% @end  --
do_top_hits(Tweet = #tweet{emotions = TweetEmos}, TopHits) ->
    TweetLevels = TweetEmos#emotions.levels,
    CmpHits = fun(Emo) ->
        % Add in the new Tweet, which may give us one too many "hits"
        TweetLevel = maps:get(Emo, TweetLevels,  0.0),
        Candidates = [{TweetLevel, Tweet} | maps:get(Emo, TopHits, [])],
        NewHits = if
            length(Candidates) > ?MAX_TOP_HITS ->
                tl(lists:sort(Candidates));
            true ->
                lists:sort(Candidates)
        end,
        {Emo, NewHits}
        end,
    relevel(CmpHits).



%%--------------------------------------------------------------------
-spec is_stoic(Emos :: tweet()
                     | emotions()) -> boolean().
%
% @doc  Returns an true if the entity has no emotion, false otherwise.
% @end  --
is_stoic(#tweet{emotions = Emos}) ->
    is_stoic(Emos);


is_stoic(#emotions{levels = Levels}) ->
    Fn = fun Recur([]) ->
                 true;
             Recur([{_, Level} | RestLevels]) ->
                 if Level > ?EPSILON -> false;
                    true             -> Recur(RestLevels)
                 end end,
    Fn(maps:to_list(Levels)).



%%--------------------------------------------------------------------
-spec relevel(Closure :: fun((atom()) -> {atom(), float()})) -> map().
%
% @doc  Applies the Closure function to all emotion values in the
%       (wrapped) input map.
%
%       NOTE: Although this function is intended to recalculate emotion
%             levels for a tweet or a collection of tweets, it can be
%             used generically to apply all Sila's emotion atoms to any
%             given function/closure.
% @end  --
relevel(Closure) ->
    Levels = lists:map(Closure, ?EMOTIONS),
    %?debug("LEVELS: ~p", [Levels]),
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
