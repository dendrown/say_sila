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
-module(sila).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start/0, start/1,stop/0, reset/0]).


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start() -> ok
            | {error, term()}.
%
% @doc  Easy startup for `say_sila` application.
% @end  --
start() ->
    start([]).



%%--------------------------------------------------------------------
-spec start(Options :: atom | list()) -> ok
                                       | {error, term()}.
%
% @doc  Easy startup for `say_sila` application with `Options'.
%       Currently, the only supported option is `twitter', which will
%       start the authorization process to track Twitter tweets.
% @end  --
start(Option) when is_atom(Option) ->
    start([Option]);


start(Options) ->
    case application:start(say_sila) of
        ok ->
            % We're currently handling only semi-auto-login
            case proplists:get_value(twitter, Options) of
                true      -> twitter:login();
                undefined -> ok
            end,
            ok;

        Bummer ->
            Bummer
    end.



%%--------------------------------------------------------------------
-spec stop() -> ok
              | {error, term()}.
%
% @doc  Say Sila shutdown
% @end  --
stop() ->
    application:stop(say_sila).



%%--------------------------------------------------------------------
-spec reset() -> list({atom(), list()}).
%%
% @doc  Reinitializes the state of the `say_sila' application.
%%--------------------------------------------------------------------
reset() ->
    Modules  = [raven, player],
    Trackers = [cc, gw],
    ResetTrk = fun(Trk) -> {Trk, [{Mod, Mod:reset(Trk)} || Mod <- Modules]} end,
    lists:map(ResetTrk, Trackers).



%%====================================================================
%% Internal functions
%%====================================================================
