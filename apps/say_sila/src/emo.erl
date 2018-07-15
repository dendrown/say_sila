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
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(emo).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([add/2,
         average/1,
         average/2,
         clip_stoic/1,
         do_top_hits/2,
         emote/4,
         emote/5,
         is_stoic/1,
         relevel/1,
         stoic/0,
         stoic/1]).

-include("emo.hrl").
-include("twitter.hrl").
-include_lib("llog/include/llog.hrl").

-define(EPSILON, 0.00000001).


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec add(Emos1 :: emos(),
          Emos2 :: emos()) -> emos().
%
% @doc  Combines the two `emos' records by adding the values for
%       each individual emotion.
% @end  --
add(Emos1, Emos2) ->
    #emos{count  = Emos1#emos.count
                 + Emos2#emos.count,
          levels = relevel(fun(Emo) -> {Emo,  maps:get(Emo, Emos1#emos.levels)
                                            + maps:get(Emo, Emos2#emos.levels)} end)}.



%%--------------------------------------------------------------------
-spec average(Emos :: emos()) -> emos().
%
% @doc  Returns an `emos' record representing the average emotion
%       levels per tweet.
%
%       CAUTION: Currently nothing is stopping us from whittling away
%                emotional levels by calling this function repeatedly
%                for the same group!
% @end  --
average(Emos = #emos{count = Cnt}) ->
    if  0 =:= Cnt -> #emos{};
        1 =:= Cnt -> Emos;
        Cnt > 1 ->
            #emos{count  = Cnt,
                  levels = relevel(fun(Emo) -> {Emo, maps:get(Emo, Emos#emos.levels) / Cnt} end)}
    end.



%%--------------------------------------------------------------------
-spec average(Running  :: emos(),
              Incoming :: emos()) -> emos().
%
% @doc  Updates a running average `emos' record with new (incoming)
%       levels of emotion.
% @end  --
average(Running  = #emos{count = RunCnt},
        Incoming = #emos{count = IncCnt}) ->
    %
    TotalCnt = RunCnt + IncCnt,
    Averager = fun(Emo) ->
                      Run = maps:get(Emo, Running#emos.levels),
                      Inc = maps:get(Emo, Incoming#emos.levels),
                      {Emo, (RunCnt * Run + IncCnt * Inc) / TotalCnt}
                      end,
    #emos{count  = TotalCnt,
              levels = relevel(Averager)}.



%%--------------------------------------------------------------------
-spec clip_stoic(Tweet :: tweet()) -> tweet().
%
% @doc  Returns the specified tweet, changing the text field field to
%       `ignored' if all tweet levels are zero.
%       levels per tweet.
% @end  --
clip_stoic(Tweet) ->
    %
    case emo:is_stoic(Tweet) of
        true  -> Tweet#tweet{text = ignored};
        false -> Tweet
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
    TweetLevels = TweetEmos#emos.levels,
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
-spec emote(Anger   :: float(),
            Fear    :: float(),
            Sadness :: float(),
            Joy     :: float()) -> emos().
%
% @doc  Returns an `emotion' record representing a single communication
%       at the specifed levels of emotion.
%
%       Equivalent to emote(1, Anger, Fear, Sadness, Joy).
% @end  --
emote(Anger, Fear, Sadness, Joy) ->
    emote(1, Anger, Fear, Sadness, Joy).



%%--------------------------------------------------------------------
-spec emote(Count   :: non_neg_integer(),
            Anger   :: float(),
            Fear    :: float(),
            Sadness :: float(),
            Joy     :: float()) -> emos().
%
% @doc  Returns an `emotion' record representing `Count' communications
%       of the specifed levels of emotion.
% @end  --
emote(Count, Anger, Fear, Sadness, Joy) ->
    #emos{count   = Count,
          levels  = #{anger   => Anger,
                      fear    => Fear,
                      sadness => Sadness,
                      joy     => Joy}}.



%%--------------------------------------------------------------------
-spec is_stoic(Emos :: tweet()
                     | emos()
                     | float()) -> boolean().
%
% @doc  Returns an true if the entity has no emotion, false otherwise.
% @end  --
is_stoic(#tweet{emotions = Emos}) ->
    is_stoic(Emos);


is_stoic(#emos{levels = Levels}) ->
    Fn = fun Recur([]) ->
                 true;
             Recur([{_, Level} | RestLevels]) ->
                 if Level > ?EPSILON -> false;
                    true             -> Recur(RestLevels)
                 end end,
    Fn(maps:to_list(Levels));


is_stoic(Level) when is_float(Level) ->
    (Level < ?EPSILON).



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
-spec stoic() -> emos().
%
% @doc  Returns an `emotion' record representing "no emotion".
%
%       Equivalent to stoic(1).
% @end  --
stoic() ->
    stoic(1).


%%--------------------------------------------------------------------
-spec stoic(Count :: non_neg_integer()) -> emos().
%
% @doc  Returns an `emotion' record representing "no emotion" for the
%       specified communication count.
% @end  --
stoic(Count) ->
    emote(Count, 0.0, 0.0, 0.0, 0.0).



%%====================================================================
%% Internal functions
%%====================================================================
