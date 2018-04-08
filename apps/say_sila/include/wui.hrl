%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Web User Interface (WUI) server for Say-Sila
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_wui_included).
-define(_wui_included, ack).

-include_lib("yaws/include/yaws_api.hrl").
-type arg() :: #arg{}.


% Configuration specific to embedded YAWS
-record(yaws_conf, {id          :: binary() | string(),
                    gConf       :: tuple(),
                    sConfs      :: [tuple()],
                    childSpecs  :: [tuple()]}).
-type yaws_conf() :: #yaws_conf{}.


-define(WUI_CENTER, {style,   <<"text-align: center">>}).
-define(WUI_LEFT,   {style,   <<"text-align: left">>}).
-define(WUI_RIGHT,  {style,   <<"text-align: right">>}).
-define(WUI_CSPAN2, {colspan, <<"2">>}).

-define(wui_cspan(X), {colspan, ??X}).
-define(wui_rspan(X), {rowspan, ??X}).

-endif.
