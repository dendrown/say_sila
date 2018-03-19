%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Charting utilities, based on gnuplot
%%
%% @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(plot).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([plot/1]).


-include("sila.hrl").
-include_lib("llog/include/llog.hrl").

-define(PLOT_DIR, "/tmp/sila/plot").
-define(TERM_DIR, ?str_fmt("~s/plot", [code:priv_dir(say_sila)])).

-define(fmt(Fmt, Args), io_lib:format(Fmt, Args)).

%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec plot(Parms :: map()) -> {ok, string()}
                            | {{error, term()}, string()}.
%%
% @doc  Creates an image file for a gnuplot created from the parameter
%       map.
% @end  --
plot(Parms = #{name := Name,
               data := Data}) ->

    {FPath, FOut} = open(Name),

    ?io_fmt(FOut, "set title  '~s'~n",    [maps:get(title,  Parms, <<"Say Sila">>)]),
    ?io_fmt(FOut, "set xlabel '~s'~n",    [maps:get(xlabel, Parms, <<"X">>)]),
    ?io_fmt(FOut, "set ylabel '~s'~n",    [maps:get(ylabel, Parms, <<"Y">>)]),

    % TODO: don't assume plot type
    ?io_put(FOut, "set style data histogram\n"),
    ?io_put(FOut, "set boxwidth 1.4\n"),
    ?io_put(FOut, "set xtic auto\n"),
    ?io_put(FOut, "set ytic auto\n"),

    Ranger = fun(Rng) ->
                 [Axe|_] = atom_to_list(Rng),
                 case maps:get(Rng, Parms, auto) of
                     auto  -> ?io_fmt(FOut, "set auto ~c~n", [Axe]);
                     {L,H} -> ?io_fmt(FOut, "set ~s [~B:~B]~n", [Rng, L, H])
                end end,
    lists:foreach(Ranger, [xrange, yrange]),

    % TODO: marker lines will need some flexibility too
    Marker = fun(X) ->
                 ?io_fmt(FOut, "set arrow from ~B, graph 0 to ~B, graph 1 nohead lc 3 lt 5~n", [X, X])
                 end,
    lists:foreach(Marker, maps:get(markers, Parms, [])),

    ?io_put(FOut, "set terminal png size 800,600 enhanced font 'Helvetica,14'\n"),
    ?io_fmt(FOut, "set output '~s/~s.png'~n", [?TERM_DIR, Name]),
    ?io_nl(FOut),
    ?io_fmt(FOut, "plot '~s' using 1 title '~s'~n", [Data, maps:get(dtitle, Parms, <<"Data">>)]),

    close(FPath, FOut),
    exec(FPath);


plot(_) ->
    {{error, bad_parms}, ""}.



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec make_fpath(Name :: string()) -> string().
%%
% @doc  Returns the full filepath for a gnuplot script file with the
%       specified name.  Note that the path must be valid for this
%       function to succeed.
% @end  --
make_fpath(Name) ->
    FPath = lists:flatten(io_lib:format("~s/~s.plt", [?PLOT_DIR, Name])),
    ok = filelib:ensure_dir(FPath),
    FPath.



%%--------------------------------------------------------------------
-spec open(Name :: string()) -> {string(), term()}.
%%
% @doc  Opens a gnuplot script file and writes out a header.
% @end  --
open(Name) ->
    %
    FPath = make_fpath(Name),
    {ok, FOut} = file:open(FPath, [write]),

    ?io_fmt(FOut, "#!~s gnuplot~n#~n", [os:find_executable("env")]),
    ?io_fmt(FOut, "# Generated by Say Sila at ~s~n~n", [dts:str(erlang:localtime())]),
    ?io_put(FOut, "unset logscale\n"),
    ?io_put(FOut, "unset label\n\n"),
    {FPath, FOut}.



%%--------------------------------------------------------------------
-spec close(FPath  :: string(),
            FOut   :: pid()) -> {ok, string()}
                              | {{error, atom()}, string()}.
%%
% @doc  Closes a gnuplot file
% @end  --
close(FPath, FOut) ->
    FStatus = file:close(FOut),
    ?info("PLOT<create>: path[~s] stat[~p]", [FPath, FStatus]),
    {FStatus, FPath}.



%%--------------------------------------------------------------------
-spec exec(ScriptPath :: string()) -> ok
                                    | bad_cmd.
%%
% @doc  Runs gnuplot on the script specified by `FPath'.
% @end  --
exec(ScriptPath) ->
    case os:find_executable("gnuplot") of
        false    -> bad_cmd;
        ExecPath ->
            _ExecOut = os:cmd(?str_fmt("~s ~s", [ExecPath, ScriptPath]))
            %?debug("GNUPLOT: ~s", [_ExecOut])
    end.