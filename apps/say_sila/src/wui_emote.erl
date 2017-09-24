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

-export([out/1, out/2]).

-include("llog.hrl").
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
    out(Arg, yaws_api:queryvar(Arg, "emo")).



%%--------------------------------------------------------------------
-spec out(Arg :: arg(),
          Emo :: binary() | string()) -> {html,  term()}
                                       | {ehtml, term()}.
%%
% @doc  Returns the WUI output for YAWS for emotion reporting as requested
%       by the URL `querydata'.
% @end  --
out(Arg, Emo) when is_atom(Emo) ->
    out(Arg, atom_to_list(Emo));


out(Arg, Emo) ->
    %
    % TODO: Have raven check for valid emotion (else spock)
    %?debug("EMO: ~p", [Emo]),
    Track = get_track(Arg),
    ImgSrc = if
        Track =/=  undefined andalso
        Emo   =/= "undefined" ->
            ["graph/", Track, ".15.day.", Emo, ".png"];

        true ->
            <<"image/spock.png">>
    end,
    {ehtml, {img, [{src,   ImgSrc},
                   {class, <<"img-fluid">>},
                   {alt,   [Emo, <<" analysis">>]}]}}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec get_track(Arg :: arg()) -> binary().
%%
% @doc  Returns the tracking code requested in the URL `querydata'.
% @end  --
get_track(Arg) ->
    case yaws_api:queryvar(Arg, "track") of
        {ok, "cc"}  -> <<"cc">>;
        {ok, "gw"}  -> <<"gw">>;
        _           -> undefined
    end.
