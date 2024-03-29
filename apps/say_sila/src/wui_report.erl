%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Web User Interface (WUI) main report page
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(wui_report).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([out/1]).

-include("sila.hrl").
-include("raven.hrl").
-include("wui.hrl").
-include_lib("llog/include/llog.hrl").


-define(CENTER, {style,   <<"text-align: center">>}).
-define(LEFT,   {style,   <<"text-align: left">>}).
-define(RIGHT,  {style,   <<"text-align: right">>}).
-define(CSPAN2, {colspan, <<"2">>}).


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
    % Big/Reg reports come as proplist triples
    {BigRpts, RegRpts} = wui:get_reports(Arg),

    % Are we doing the full/tweet/retweet version?
    Comms  = wui:get_comms_atom(Arg, full),
    BigRpt = proplists:get_value(Comms, BigRpts),
    RegRpt = proplists:get_value(Comms, RegRpts),

    BegDTS  = element(1, dts:earlier(BigRpt#report.beg_dts, RegRpt#report.beg_dts)),
    EndDTS  = element(1, dts:later(BigRpt#report.end_dts, RegRpt#report.end_dts)),
    RptSpan = io_lib:format("Report Span: ~s &ndash; ~s", [dts:str(BegDTS),
                                                           dts:str(EndDTS)]),
    {ehtml, {table, [{class, <<"table table-sm">>}],
                    [{thead, [],
                             [{th, [?LEFT], RptSpan},
                              {th, [?LEFT], <<"Big Players">>},
                              {th, [?LEFT], <<"Regular Players">>}]},
                     {tbody, [],
                            [{tr, [],
                                  [{td, [?RIGHT], <<"<b>Player Count:</b>">>},
                                   {td, [?LEFT], io_lib:format("~B", [BigRpt#report.num_players])},
                                   {td, [?LEFT], io_lib:format("~B", [RegRpt#report.num_players])}]},
                             {tr, [],
                                  [{td, [?RIGHT], <<"<b>Tweet Count:</b>">>},
                                   {td, [?LEFT], io_lib:format("~B", [BigRpt#report.num_tweets])},
                                   {td, [?LEFT], io_lib:format("~B", [RegRpt#report.num_tweets])}]} ]}]}}.



%%====================================================================
%% Internal functions
%%====================================================================
