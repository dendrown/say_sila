%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Tweet Emotion Analyzer for Climate Change
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(emo_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([add_01/1,
         average_01/1, average_02/1,
         emote_01/1]).

-include("emo.hrl").

%%%-------------------------------------------------------------------
-define(ANGER,  #emotions{count  = 1,
                          levels = #{anger   => 1.0,
                                     fear    => 0.0,
                                     joy     => 0.0,
                                     sadness => 0.0}}).

-define(FEAR,   #emotions{count  = 1,
                          levels = #{anger   => 0.0,
                                     fear    => 1.0,
                                     joy     => 0.0,
                                     sadness => 0.0}}).

-define(JOY,    #emotions{count  = 1,
                          levels = #{anger   => 0.0,
                                     fear    => 0.0,
                                     joy     => 1.0,
                                     sadness => 0.0}}).

-define(SADNESS,#emotions{count  = 1,
                          levels = #{anger   => 0.0,
                                     fear    => 0.0,
                                     joy     => 0.0,
                                     sadness => 1.0}}).


%%--------------------------------------------------------------------
all() ->
    [add_01,
     average_01, average_02,
     emote_01].


%%--------------------------------------------------------------------
add_01(_Config) ->
    AFJS = all_emos(),
    4 = AFJS#emotions.count,
    [1.0, 1.0, 1.0, 1.0] = to_emo_list(AFJS).


%%--------------------------------------------------------------------
average_01(_Config) ->
    AFJS = all_emos(),
    Avg  = emo:average(AFJS),
    4 = AFJS#emotions.count,
    [0.25, 0.25, 0.25, 0.25] = to_emo_list(Avg).


%%--------------------------------------------------------------------
average_02(_Config) ->
    A2 = emo:average(?ANGER, ?FEAR),
    2  = A2#emotions.count,
    [0.50, 0.50, 0.00, 0.00] = to_emo_list(A2),

    A3 = emo:average(A2, ?JOY),
    A4 = emo:average(A3, ?SADNESS),
    4  = A4#emotions.count,
    [0.25, 0.25, 0.25, 0.25] = to_emo_list(A4),

    M4 = emo:average(all_emos()),
    A8 = emo:average(A4, M4),
    8  = A8#emotions.count,
    [0.25, 0.25, 0.25, 0.25] = to_emo_list(A8).


%%--------------------------------------------------------------------
emote_01(_Config) ->
    Emo = emo:emote(0.0, 0.0, 0.0, 0.0),
    emo:is_stoic(Emo).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
all_emos() ->
    AF   = emo:add(?ANGER, ?FEAR),
    AFJ  = emo:add(AF,     ?JOY),
    AFJS = emo:add(AFJ,    ?SADNESS),
    AFJS.


%%--------------------------------------------------------------------
to_emo_list(Emos) ->
    lists:map(fun(E) -> maps:get(E, Emos#emotions.levels) end, ?EMOTIONS).
