%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila code relating to research on the "Big Players"
%%
%% @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(biggies).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([bp/1,
         run_top_nn/2]).

-include("sila.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


%%--------------------------------------------------------------------
%% Period shortcuts:
%%
%% RUN-1: October 1, 2017 -- June 30, 2018              (10-fold CV)
%%--------------------------------------------------------------------
bp(parms)   -> [{start, {2019, 01, 01}}, {stop, {2019, 04, 01}}];
bp(train)   -> [{start, {2017, 10, 01}}, {stop, {2018, 07, 01}}];   % FIXME!
bp(test)    -> [{start, {2019, 10, 01}}, {stop, {2019, 12, 31}}].


%%--------------------------------------------------------------------
-spec run_top_nn(Tracker :: tracker(),
                 RunTag  :: stringy()) -> proplist().
%%
% @doc  Do the Twitter/Emo influence experiments using the specified
%       tracker and selecting the Top N big-player accounts across a
%       range of Ns for one emotion and communication type.
%
%       This `nn' function goes through the Top-N twice, using only
%       the emo/comm attributes from the higher ranking models.
%
%       This function uses weekly periods.
% @end  --
run_top_nn(Tracker, RunTag) ->

    io:format("This will destroy the current raven and player states."),
    case ioo:read_down("Are you sure? ") of
        "yes" ->
            case raven:get_jvm_node(Tracker) of
                false -> [];
                true  ->
                    raven:reset(Tracker),
                    raven:emote(Tracker, bp(train)),
                    influence:run_top_nn(Tracker, RunTag, oter, fear)
                end;
        _ -> []
    end.
