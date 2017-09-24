%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Web User Interface (WUI) page for reporting emotion content.
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(wui_emote).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").
-type arg() :: #arg{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec out(Arg :: arg()) -> {html,  term()}
                         | {ehtml, term()}.
%%
% @doc  Returns the WUI output for YAWS for emotion reporting as requested
%       by the URL `querydata'.
% @end  --
out(Arg) ->
   {ehtml, {img, [{src,   ["graph/", get_track(Arg), ".15.day.anger.png"]},
                  {class, <<"img-fluid">>},
                  {alt,   <<"Fear Analysis">>}]}}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec get_track(Arg :: arg()) -> binary().
%%
% @doc  Returns the tracking code requested in the URL `querydata'.
% @end  --
get_track(#arg{querydata = QryData}) ->
    %
    % FIXME: We need to parse the thing...really soon!
    case QryData of
        "track=cc"  -> <<"cc">>;
        _           -> <<"gw">>
    end.
