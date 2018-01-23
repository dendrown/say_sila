%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Tweet Emotion Analyzer for Climate Change
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(say_sila_app).
-behaviour(application).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start/2, stop/1]).

-include_lib("llog/include/llog.hrl").

%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start(StartType :: permanent | transient | temporary,
            StartArgs :: term()) -> ok
                                  | {error, term()}.
%
% @doc  Say Sila startup
% @end  --
start(_StartType, _StartArgs) ->
    llog:start(),
    ?notice("Say hello to Say Sila"),
    %?debug("Type: ~p", [_StartType]),
    %?debug("Args: ~p", [_StartArgs]),

    % Setup for Twitter
    lists:foreach(fun(App) -> ok = application:ensure_started(App) end,
                  [inets,
                   crypto,
                   asn1,
                   public_key,
                   ssl,
                   oauth    %,
                  %----------
                  % Prepping for hackey...
                  %----------
                  %unicode_util_compat,
                  %idna,
                  %mimerl,
                  %certifi,
                  %ssl_verify_fun,
                  %metrics,
                  %hackney
                  ]),
    Return = say_sila_sup:start_link(),
    case Return of
        {ok, _} -> wui:configure();
        _       -> ?warning("Not launching user interface due to abnormal startup")
    end,
    Return.



%%--------------------------------------------------------------------
-spec stop(State :: term()) -> ok
                             | {error, term()}.
%
% @doc  Say Sila shutdown
% @end  --
stop(_State) ->
    ?notice("Say goodbye now"),
    ok.



%%====================================================================
%% Internal functions
%%====================================================================
