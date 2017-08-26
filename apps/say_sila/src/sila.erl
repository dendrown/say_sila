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
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-module(sila).

-export([go/0, go/1, stop/0]).


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec go() -> ok
            | {error, term()}.
%
% @doc  Easy startup for Say Sila
% @end  --
go() ->
    go([]).



%%--------------------------------------------------------------------
-spec go(Options :: atom | list()) -> ok
                                    | {error, term()}.
%
% @doc  Easy startup for Say Sila with `Options'.  Currently, the only
%       supported option is `twitter', which will start the authorization
%       process to track Twitter tweets.
% @end  --
go(Option) when is_atom(Option) ->
    go([Option]);


go(Options) ->
    case application:start(say_sila) of
        ok ->
            % We're currently handling only semi-auto-login
            case proplists:get_value(twitter, Options) of
                true      -> nest:connect();
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



%%====================================================================
%% Internal functions
%%====================================================================
