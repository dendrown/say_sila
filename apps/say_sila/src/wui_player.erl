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
-include("player.hrl").
-include("wui.hrl").
-include_lib("llog/include/llog.hrl").


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
    P100   = 0.4,                           % TODO: Do [10..50]
    Track  = case wui:get_track(Arg) of     % TODO: Handled returned binary
        _ -> gw
    end,
    Counts = player:get_big_venn(Track, P100),
    Rower  = fun({Comms, Cnt}) ->
                 CommsUp  = lists:map(fun(X) -> string:uppercase(atom_to_list(X)) end, Comms),
                 CommsID  = lists:join("_", ["cnt" | CommsUp]),
                 CommsTxt = lists:join(", ", CommsUp),
                 {tr, [],
                      [{td, [?WUI_LEFT],                 CommsTxt},
                       {td, [?WUI_RIGHT, {id, CommsID}], integer_to_binary(Cnt)}]}
                 end,

    {ehtml, {table, [],
                    [{thead, [],
                             [{th, [?WUI_CENTER], <<"Venn">>},
                              {th, [?WUI_LEFT],   <<"Comm(s)">>},
                              {th, [?WUI_CENTER], <<"Count">>}]},
                     {tbody, [],
                             [{td, [{id,      venn},
                                    {width,   600},
                                    {rowspan, integer_to_binary(1 + length(Counts))}],
                                   <<"&nbsp;">>}
                             | [Rower(CC) || CC <- Counts]]} ]}}.



%%====================================================================
%% Internal functions
%%====================================================================
