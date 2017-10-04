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
-include("raven.hrl").
-include("twitter.hrl").
-include("wui.hrl").



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
    Track = wui:get_track(Arg),
    ImgSrc = if
        Track =/=  undefined andalso
        Emo   =/= "undefined" ->
            io_lib:format("graph/~s.~s.png", [wui:get_tag(Track), Emo]);

        true ->
            <<"image/spock.png">>
    end,
    {BigRpt, RegRpt} = wui:get_reports(Arg),
    {ehtml, [{h2,    [], string:to_upper(Emo)},
             {img,   [{src,   ImgSrc},
                      {class, <<"img-fluid">>},
                      {alt,   [Emo, <<" analysis">>]}]},
             {br,    []},
             {br,    []},
             {table, [{class, <<"table table-sm">>}],
                     [{thead, [], {h4, [], <<"Big Player Top Tweets">>}},
                      {tbody, [], get_top_hit_trs(Emo, BigRpt#report.top_hits)}]},
             {br,    []},
             {table, [{class, <<"table table-sm">>}],
                     [{thead, [], {h4, [], <<"Regular Player Top Tweets">>}},
                      {tbody, [], get_top_hit_trs(Emo, RegRpt#report.top_hits)}]} ]}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec get_top_hit_trs(Emo     :: atom() | string(),
                      TopHits :: map()) -> [tuple()].
%%
% @doc  Returns `ehtml' (tr/td) table rows representing the highest level
%       tweets for the specified emotion.
%
%       FIXME: The main `out' function changes the emotion atom to a list,
%              and here we change it back to an atoms.  Surely we can do
%              better.
%
% @end  --
get_top_hit_trs(Emo, TopHits) when is_list(Emo) ->
    get_top_hit_trs(list_to_existing_atom(Emo), TopHits);


get_top_hit_trs(Emo, TopHits) ->
    case maps:get(Emo, TopHits, undefined) of
        undefined ->
            {tr, [], {td, [], io_lib:format("No tweets for ~s", [Emo])}};

        EmoHits ->
            lists:map(fun({Level, #tweet{text        = Text,
                                         screen_name = Player}}) ->
                          {tr, [],
                               [{td, [], Player},
                                {td, [], Text},
                                {td, [], io_lib:format("~g", [Level])}]}
                          end,
                          lists:reverse(EmoHits))
    end.
