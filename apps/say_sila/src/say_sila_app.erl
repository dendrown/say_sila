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

-include("sila.hrl").
-include_lib("llog/include/llog.hrl").

%%====================================================================
%% API
%%--------------------------------------------------------------------
%% start:
%
% @doc  Say Sila startup
% @end  --
start(_StartType, _StartArgs) ->
    llog:start(),
    {ok, App} = application:get_application(),

    ?notice("Say hello to Say Sila"),
    %?debug("Type: ~p", [_StartType]),
    %?debug("Args: ~p", [_StartArgs]),

    init_mnesia(App),
    init_dependencies(),

    % Setup for Twitter
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
%%--------------------------------------------------------------------
-spec init_mnesia(App :: atom()) -> ok.
%
% @doc  Launch Mnesia and create the schema if necessary
% @end  --
init_mnesia(App) ->
    %
    case application:get_env(App, mnesia, undefined) of
        undefined ->
            ?warning("Missing mnesia configuration");

        Mnesia ->
            % Check that node is in the distributed DB group
            %
            % NOTE: We'll be sharing nodes between [gw, cc, ui]
            Nodes = proplists:get_value(nodes, Mnesia),
            Node  = node(),
            case lists:member(node(), Nodes) of

                false ->
                    ?info("Node ~s is not in the mnesia distributed node group", [Node]);

                true ->
                    application:set_env(mnesia, dir, [?WORK_DIR, "/Mnesia.", Node]),
                    case mnesia:create_schema(Nodes) of
                        ok ->
                            ?debug("Mnesia schema created: dist~p", [Nodes]);

                        {error, {_, {already_exists, _}}} ->
                            ?debug("Using existing Mnesia schema: dist~p", [Nodes])
                    end,
                    ok = mnesia:start(),
                    ?info("Mnesia initialized: run[~s] dir[~s]", [mnesia:system_info(is_running),
                                                                  mnesia:system_info(directory)])
            end
        end.



%%--------------------------------------------------------------------
-spec init_dependencies() -> ok.
%
% @doc  Launch Mnesia and create the schema if necessary
% @end  --
init_dependencies() ->
    lists:foreach(fun(Dep) -> ok = application:ensure_started(Dep) end,
                  [inets,
                   crypto,
                   asn1,
                   public_key,
                   ssl,
                   oauth    %,
                  %----------
                  % TODO: decide httpc vs. hackey for auto-auth on Twitter
                  %----------
                  %unicode_util_compat,
                  %idna,
                  %mimerl,
                  %certifi,
                  %ssl_verify_fun,
                  %metrics,
                  %hackney
                  ]).
