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
%% @copyright 2018 Dennis Drown and Ostrich Ideas
%% @end
%%%-------------------------------------------------------------------
-ifndef(_ioo_included).
-define(_ioo_included, ack).

-define(str_fmt(Fmt, Args),     io_lib:format(Fmt, Args)).
-define(str_FMT(Fmt, Args),     lists:flatten(?str_fmt(Fmt, Args))).
-define(bin_fmt(Fmt, Args),     list_to_binary(?str_fmt(Fmt, Args))).
-define(fmt(Fmt),               io:format(Fmt)).
-define(fmt(Fmt, Args),         io:format(Fmt, Args)).
-define(io_fmt(Out, Fmt, Args), io:format(Out, Fmt, Args)).
-define(io_put(FOut, Fmt),      io:put_chars(FOut, Fmt)).
-define(io_nl(FOut),            io:put_chars(FOut, "\n")).

-endif.
