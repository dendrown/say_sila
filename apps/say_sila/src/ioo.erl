%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc I/O Operations
%%
%% @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(ioo).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([make_fpath/3,
         put_all/2,
         read_down/1,
         trim_down/1]).


-include("ioo.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec make_fpath(Path :: stringy(),
                 Name :: stringy(),
                 Extn :: stringy()) -> string().
%%
% @doc  Returns the full filepath for a gnuplot script file with the
%       specified name.  Note that the path must be valid for this
%       function to succeed.
% @end  --
make_fpath(Path, Name, Extn) ->
    FPath = lists:flatten(io_lib:format("~s/~s.~s", [Path, Name, Extn])),
    ok = filelib:ensure_dir(FPath),
    FPath.



%%--------------------------------------------------------------------
-spec put_all(FOut :: pid(),
              Cmds :: [string()]) -> ok.
%%
% @doc  Adds a list of commands, one per line, to the open file `FOut'.
% @end  --
put_all(FOut, Cmds) ->
    lists:foreach(fun(Cmd) -> ?io_fmt(FOut, "~s\n", [Cmd]) end, Cmds).



%%--------------------------------------------------------------------
-spec read_down(Prompt :: io:prompt()) -> string().
%%
% @doc  Displays the `Prompt', requests user input, and returns the
%       input text after trimming and converting it to lowercase.
% @end  --
read_down(Prompt) ->
    Reply = io:get_line(Prompt),
    trim_down(Reply).



%%--------------------------------------------------------------------
-spec trim_down(Text :: unicode:chardata()) -> string().
%%
% @doc  Trims whitespace from both sides of the specified `Text' and
%       converts it to lowercase.
% @end  --
trim_down(Text) ->
    string:lowercase(string:trim(Text)).



%%====================================================================
%% Internal functions
%%====================================================================
