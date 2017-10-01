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
-include("llog.hrl").
-include("wui.hrl").


-define(LEFT , {style, <<"text-align: left">>}).
-define(RIGHT, {style, <<"text-align: right">>}).


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
    Span = case wui:get_track(Arg) of
        undefined ->
            <<"Logic is the beginning of wisdom, not the end">>;

        Track ->
            StatDir = wui:get_status_dir(),
            FileTag = wui:get_tag(Track),
            {ok, BegTxt} = file:read_file(io_lib:format("~s/~s.begin.txt", [StatDir, FileTag])),
            {ok, EndTxt} = file:read_file(io_lib:format("~s/~s.end.txt",   [StatDir, FileTag])),
            io_lib:format("~s &ndash; ~s", [BegTxt, EndTxt])
    end,
    {ehtml, {table, [{class, <<"table table-sm">>}],
                    {tbody, [],
                            [{tr, [], [{td, [?RIGHT], <<"<b>Report Span:</b>">>}, {td, [?LEFT], Span}]},
                             {tr, [], [{td, [?RIGHT], <<"<b>Tweet Count:</b>">>}, {td, [?LEFT], "******"}]}]}}}.



%%====================================================================
%% Internal functions
%%====================================================================
