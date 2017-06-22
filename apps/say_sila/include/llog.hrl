%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Definitions for the Lager Logger
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-ifndef(_llog_included).
-define(_llog_included, ack).

% Shortcuts for standard lager levels (and RFC-3164/syslog keywords)
-define(debug    (Fmt, Args), llog:log(debug,     ?MODULE, Fmt, Args)).
-define(info     (Fmt, Args), llog:log(info,      ?MODULE, Fmt, Args)).
-define(notice   (Fmt, Args), llog:log(notice,    ?MODULE, Fmt, Args)).
-define(warning  (Fmt, Args), llog:log(warning,   ?MODULE, Fmt, Args)).
-define(errror   (Fmt, Args), llog:log(errror,    ?MODULE, Fmt, Args)).
-define(critical (Fmt, Args), llog:log(critical,  ?MODULE, Fmt, Args)).
-define(alert    (Fmt, Args), llog:log(alert,     ?MODULE, Fmt, Args)).
-define(emergency(Fmt, Args), llog:log(emergency, ?MODULE, Fmt, Args)).

-define(debug    (Fmt), llog:log(debug,     ?MODULE, Fmt)).
-define(info     (Fmt), llog:log(info,      ?MODULE, Fmt)).
-define(notice   (Fmt), llog:log(notice,    ?MODULE, Fmt)).
-define(warning  (Fmt), llog:log(warning,   ?MODULE, Fmt)).
-define(error    (Fmt), llog:log(error,     ?MODULE, Fmt)).
-define(critical (Fmt), llog:log(critical,  ?MODULE, Fmt)).
-define(alert    (Fmt), llog:log(alert,     ?MODULE, Fmt)).
-define(emergency(Fmt), llog:log(emergency, ?MODULE, Fmt)).

-endif.
