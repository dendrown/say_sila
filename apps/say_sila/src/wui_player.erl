%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Web User Interface (WUI) player information page
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(wui_player).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([out/1]).

-include("sila.hrl").
-include("ioo.hrl").
-include("player.hrl").
-include("wui.hrl").
-include_lib("llog/include/llog.hrl").

-define(BIG_P100s, [0.1, 0.2, 0.3, 0.4, 0.5]).


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec out(Arg :: arg()) -> {html,  term()}
                         | {ehtml, term()}.
%%
% @doc  Returns the WUI output for YAWS for player reporting as requested
%       by the URL `querydata'.
% @end  --
out(Arg) ->
    case wui:get_track(Arg, atom) of

        undefined ->
            % No valid track requested, give a quick menu
            Linker = fun(Trk) ->
                     {li, [],
                          {a, [{href, ?str_fmt("venn.yaws?track=~s", [Trk])}],
                              ?str_fmt("Communications for ~s", [twitter:to_hashtag(Trk)])}}
                         end,
            {ehtml, {ul, [], lists:map(Linker, [gw, cc])}};

        Track ->
            ?debug("Player track: ~p", [Track]),
            % Show me the Venns!
            Venns = lists:map(fun(P100) -> make_comms_venn(Track, P100) end, ?BIG_P100s),
            {ehtml, Venns}
    end.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec make_comms_venn(Track :: atom(),
                      P100  :: float()) -> {table,  list(), list()}.
%%
% @doc  Creates a Venn diagram and supporting table for the communication
%       code combinations given a specific tracker and inclusion percentage.
% @end  --
make_comms_venn(Track, P100) ->
    %
    Counts = player:get_big_venn(Track, P100),
    BigPct = round(100 * P100),
    Rower  = fun({Comms, Cnt}) ->
                 CommsUp  = lists:map(fun(X) -> string:uppercase(atom_to_list(X)) end, Comms),
                 CommsID  = lists:join("_", ["cnt", integer_to_binary(BigPct) | CommsUp]),
                 CommsTxt = lists:join(", ", CommsUp),
                 {tr, [],
                      [{td, [?WUI_LEFT],                 CommsTxt},
                       {td, [?WUI_RIGHT, {id, CommsID}], integer_to_binary(Cnt)},
                       {td, [],                          <<"&nbsp;">>}]}
                 end,

    {table, [{class, <<"table table-sm table-hover">>}],
            [{thead, [{class, <<"thead-dark">>}],
                     [{th, [?WUI_CENTER,
                            ?wui_cspan(4)],
                           ?str_fmt("<big>~s @ ~B%</big>", [twitter:to_hashtag(Track), BigPct])}]},
             {thead, [],
                     [{th, [?WUI_CENTER], <<"Venn">>},
                      {th, [?WUI_LEFT],   <<"Comm(s)">>},
                      {th, [?WUI_RIGHT],  <<"Count">>},
                      {th, [],            <<"&nbsp;">>}]},
             {tbody, [],
                     [{td, [{id,      ?str_fmt("venn_~B", [BigPct])},
                            {width,   650},
                            {rowspan, integer_to_binary(1 + length(Counts))}],
                           <<"&nbsp;">>}
                     | [Rower(CC) || CC <- Counts]]} ]}.
