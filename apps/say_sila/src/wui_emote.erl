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

-include("raven.hrl").
-include("twitter.hrl").
-include("wui.hrl").
-include_lib("llog/include/llog.hrl").



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
    %?debug("EMO: ~p", [Emo]),
    EMO = string:to_upper(Emo),

    % TODO: Have raven check for valid emotion (else spock)
    Track = wui:get_track(Arg),
    Comms = wui:get_comms_atom(Arg, full),
    ImgSrc = if
        Track =/=  undefined andalso
        Emo   =/= "undefined" ->
            io_lib:format("graph/~s.~s.~s.png", [wui:get_tag(Track), Comms, Emo]);

        true ->
            <<"image/spock.png">>
    end,
    % Big/Reg reports come as proplist triples
    {BigRpts, RegRpts} = wui:get_reports(Arg),

    % Are we doing the full/tweet/retweet version?
    BigRpt = proplists:get_value(Comms, BigRpts),
    RegRpt = proplists:get_value(Comms, RegRpts),

    Left = [{h2,    [], EMO},
            {img,   [{src,   ImgSrc},
                     {class, <<"img-fluid">>},
                     {alt,   [Emo, <<" analysis">>]}]}],

    BigTweets = {table, [{class, <<"table table-sm">>}],
                        [{thead, [], {h5, [], <<"Big Player Top Tweets">>}},
                         {tbody, [], get_top_hit_trs(Emo, BigRpt#report.top_hits)}]},

    RegTweets = {table, [{class, <<"table table-sm">>}],
                        [{thead, [], {h5, [], <<"Regular Player Top Tweets">>}},
                         {tbody, [], get_top_hit_trs(Emo, RegRpt#report.top_hits)}]},

    NavLinks = lists:map(fun(Size) ->
                             {Active, AriaExp} = case Size of
                                 big -> {<<" active">>, {'aria-expanded', <<"true">>}};
                                 _   -> {<<>>,          {dummy,           <<"noop">>}}
                             end,
                             {li, [{class, <<"nav-item">>}],
                                  {a, [{'class',         io_lib:format("nav-link~s",  [Active])},
                                       {'href',          io_lib:format("#~s-~s-text", [Emo, Size])},
                                       {'id',            io_lib:format("~s-~s-link",  [Emo, Size])},
                                       {'aria-controls', io_lib:format("~s-~s-text",  [Emo, Size])},
                                       AriaExp,
                                       {'data-toggle',   <<"tab">>},
                                       {'role',          <<"tab">>}],
                                      string:to_upper(atom_to_list(Size))}} 
                             end,
                         [big, reg]),

    Right = [{ul, [{id,    [Emo, "-tabs"]},
                   {class, <<"nav nav-tabs">>},
                   {role,  <<"tablist">>}],
                  NavLinks},
             {'div', [{class, <<"tab-content">>},
                      {id,    [Emo, "-tweets"]}],
                     [{'div',  [{class,              <<"tab-pane fade show active">>},
                                {id,                 [Emo, <<"-big-text">>]},
                                {'aria-labelledby',  [Emo, <<"-big-link">>]},
                                {role,               <<"tabpanel">>}],
                               BigTweets},
                      {'div',  [{class,              <<"tab-pane fade">>},
                                {id,                 [Emo, <<"-reg-text">>]},
                                {'aria-labelledby',  [Emo, <<"-reg-link">>]},
                                {role,               <<"tabpanel">>}],
                               RegTweets}]}],
    {ehtml, [{'hr'},
             {'div', [{class, <<"col">>}], Left},
             {'div', [{class, <<"col">>}], Right}]}.



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
            lists:map(fun({Level, #tweet{full_text   = Text,
                                         screen_name = Player}}) ->
                          {tr, [],
                               [{td, [], Player},
                                {td, [], Text},
                                {td, [], io_lib:format("~g", [Level])}]}
                          end,
                          lists:reverse(EmoHits))
    end.
