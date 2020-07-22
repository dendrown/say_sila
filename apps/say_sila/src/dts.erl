%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila Date/Timestamp utilities
%%
%% @copyright 2017-2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(dts).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([add/3,
         date_str/1,    date_str/2,
         date_STR/1,    date_STR/2,
         day/1,
         dayize/1,      dayize/2,
         earlier/2,
         hourize/1,
         later/2,
         minutize/1,    minutize/2,
         now/1,
         quarter/1,     quarter/2,
         scale_unix/3,
         str/1,         str/2, sub/3,
         to_datetime/2,
         to_unix/2]).

-include("dts.hrl").
-include("ioo.hrl").

-define(SECS_EPOCH,     62167219200).



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec add(DTS  :: datetime(),
          Amt  :: integer(),
          Unit :: atom()) -> datetime().
%
%     Adds the specified amout of `second', `minute', `hour', `day',
%     or `month' to DTS.
% @end  --
add(Day = {_, _, _}, Amt, Unit) ->
    add({Day, {0, 0, 0}}, Amt, Unit);


add({{Year, Month, Day}, Time = {_, _, _}}, Amt, month) ->
    {NewYear,
     NewMonth} = case Month + Amt of
        M when M <  1 -> {Year-1, 12-M};
        M when M > 12 -> {Year+1, M-12};
        M             -> {Year, M}
     end,
    {{NewYear, NewMonth, Day}, Time};


add(DTS, Amt, Unit) ->
    AmtSecs  = scale_unix(Amt, Unit, second),
    OrigSecs = calendar:datetime_to_gregorian_seconds(DTS),
    NewSecs  = OrigSecs + AmtSecs,
    calendar:gregorian_seconds_to_datetime(NewSecs).



%%--------------------------------------------------------------------
-spec date_str(DTS :: tuple()) -> io_lib:chars().
%
%     Creates a printable date as an IO list from a datetime tuple.
% @end  --
date_str({Date, {_ ,_ ,_}}) ->
    str(Date);


date_str(Date) ->
    str(Date).



%%--------------------------------------------------------------------
-spec date_str(DTS1970 :: integer(),
               Unit    :: atom()) -> io_lib:chars().
%
%     Creates an ISO 8601 printable date IO list from a unix timestamp.
% @end  --
date_str(DTS1970, Unit) ->
    date_str(to_datetime(DTS1970, Unit)).



%%--------------------------------------------------------------------
-spec date_STR(DTS :: tuple()) -> io_lib:chars().
%
%     Creates a printable date as a string from a datetime tuple.
% @end  --
date_STR(Date) ->
    lists:flatten(date_str(Date)).



%%--------------------------------------------------------------------
-spec date_STR(DTS1970 :: integer(),
               Unit    :: atom()) -> string().
%
%     Creates an ISO 8601 printable date string from a unix timestamp.
% @end  --
date_STR(DTS1970, Unit) ->
    lists:flatten(date_str(DTS1970, Unit)).



%%--------------------------------------------------------------------
-spec day(DateTime :: datetime()) -> date().
%
% @doc  Strips off the time portion from a datetime tuple.
% @end  --
day(Day = {_, _, _}) ->
    Day;


day({Day = {_, _, _}, {_, _, _}}) ->
    Day.


%%--------------------------------------------------------------------
-spec dayize(DateTime :: tuple()) -> tuple().
%
% @doc  Zeros out the time portion from a datetime tuple.
% @end  --
dayize(Day = {_, _, _}) ->
    {Day, {0, 0, 0}};


dayize({{Year, Month, Day}, {_, _, _}}) ->
    {{Year, Month, Day}, {0, 0, 0}}.


%%--------------------------------------------------------------------
-spec dayize(DTS1970 :: integer(),
             Unit    :: atom()) -> integer().
%
% @doc  Zeros out the hours/minutes/seconds from a unix epoch timestamp.
% @end  --
dayize(DTS1970, Unit) ->
    DTS = to_datetime(DTS1970, Unit),
    to_unix(dayize(DTS), Unit).


%%--------------------------------------------------------------------
-spec earlier(DTS1 :: tuple(),
              DTS2 :: tuple()) -> tuple().
%
% @doc  Chooses the earlier of the two datetimes
% @end  --
earlier(DTS1, DTS2) ->
    if
        DTS1 =< DTS2 -> DTS1;
        true         -> DTS2
    end.



%%--------------------------------------------------------------------
-spec hourize(DateTime :: tuple()) -> tuple().
%
% @doc  Zeros out the minutes and seconds from a datetime tuple.
% @end  --
hourize({{Year, Month, Day}, {Hour, _, _}}) ->
    {{Year, Month, Day}, {Hour, 0, 0}}.



%%--------------------------------------------------------------------
-spec later(DTS1 :: tuple(),
            DTS2 :: tuple()) -> tuple().
%
% @doc  Chooses the later of the two datetimes
% @end  --
later(DTS1, DTS2) ->
    if
        DTS1 >= DTS2 -> DTS1;
        true         -> DTS2
    end.



%%--------------------------------------------------------------------
-spec minutize(DateTime :: tuple()) -> tuple().
%
% @doc  Zeros out the seconds from a datetime tuple.
% @end  --
minutize({{Year, Month, Day}, {Hour, Min, _}}) ->
    {{Year, Month, Day}, {Hour, Min, 0}}.



%%--------------------------------------------------------------------
-spec minutize(DTS1970 :: integer(),
               Unit    :: atom()) -> integer().
%
% @doc  Zeros out the seconds from a unix epoch timestamp.
% @end  --
minutize(DTS1970, Unit) ->
    DTS = to_datetime(DTS1970, Unit),
    to_unix(minutize(DTS), Unit).



%%--------------------------------------------------------------------
-spec now(Unit :: datetime
                | time_unit()) -> datetime()
                                | dts_1970().
%%
% @doc  Returns the current time as a datetime tuple or a unix epoch-based
%       timestamp in the specified units.
% @end  --
now(datetime) ->
    to_datetime(now(second), second);


now(Unit) ->
    if  Unit =:= second orelse
        Unit =:= millisecond  -> erlang:system_time(Unit);
        true                  -> scale_unix(erlang:system_time(second), second, Unit)
    end.



%%--------------------------------------------------------------------
-spec quarter(DateTime :: tuple()) -> atom().
%
% @doc  Returns an atom representing the yearly quarter, such as q4_2017.
% @end  --
quarter(Day = {_, _, _}) ->
    quarter({Day, {0, 0, 0}});


quarter({{Year, Month, _}, {_, _, _}}) ->
    Qtr = (Month + 2) div 3,
    list_to_atom(lists:flatten(?str_fmt("q~B_~B", [Qtr, Year]))).


%%--------------------------------------------------------------------
-spec quarter(DTS1970 :: integer(),
              Unit    :: atom()) -> atom().
%
% @doc  Returns an atom representing the yearly quarter, such as q4_2017.
% @end  --
quarter(DTS1970, Unit) ->
    quarter(to_datetime(DTS1970, Unit)).



%%--------------------------------------------------------------------
-spec scale_unix(DTS1970  :: integer(),
                 FromUnit :: time_unit(),
                 ToUnit   :: time_unit()) -> integer().
%%
%     Converts a unix epoch-based timestamp to a unix timestamps using
%     another time unit.
% @end  --
scale_unix(DTS1970, FromUnit, second) ->
    case FromUnit of
        millisecond -> DTS1970 div 1000;
        _           -> DTS1970 * get_scale(FromUnit)
    end;


scale_unix(Secs, second, ToUnit) ->
    case ToUnit of
        millisecond -> Secs * 1000;
        _           -> Secs div get_scale(ToUnit)
    end;


scale_unix(DTS1970, FromUnit, ToUnit) ->
    Secs = scale_unix(DTS1970, FromUnit, second),
    scale_unix(Secs, second, ToUnit).



%%--------------------------------------------------------------------
-spec str(DTS :: tuple()) -> string().
%
%     Creates an ISO 8601 printable string from a datetime tuple.
% @end  --
str({Year, Mon, Day}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Mon, Day]);


str({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Year, Mon, Day, Hour, Min, Sec]).



%%--------------------------------------------------------------------
-spec str(DTS1970 :: integer(),
          Unit    :: atom()) -> string().
%
%     Creates an ISO 8601 printable string from a unix timestamp.
% @end  --
str(DTS1970, Unit) ->
    str(to_datetime(DTS1970, Unit)).



%%--------------------------------------------------------------------
-spec sub(DTS  :: tuple(),
          Amt  :: integer(),
          Unit :: atom()) -> tuple().
%
%     Subtracts the specified amount of `second', `minute', `hour' or `day'
%     from DTS.
% @end  --
sub(DTS, Amt, Unit) ->
    add(DTS, -Amt, Unit).



%%--------------------------------------------------------------------
-spec to_datetime(DTS1970 :: integer(),
                  Unit    :: atom()) -> datetime().
%
%     Returns the Epoch-DTS (specify `second' or `millisecond') as a
%     datetime tuple: `{{year,mon,day},{hour,min,sec}}'
% @end  --
to_datetime(DTS1970, Unit) ->
    add(?DTS_EPOCH, DTS1970, Unit).



%%--------------------------------------------------------------------
-spec to_unix(Timestamp :: string()
                         | datetime()
                         | date(),
              Unit      :: atom()) -> non_neg_integer().
%%
%     Returns the Epoch timestamp corresponding to:
%       - a string   << "2018-02-23 14:59:05" >>
%       - a tuple   {{year,mon,day},{hour,min,sec}}

%     Specify `second' or `millisecond' for the `Unit' as needed.
% @end  --
to_unix(Stamp, Unit) when   is_binary(Stamp)
                     orelse is_list(Stamp) ->
    Parser = fun(Text, Delim) ->
                 [{A, _},
                  {B, _},
                  {C, _}] = lists:map(fun(X) -> string:to_integer(X) end, string:split(Text, Delim, all)),
                 {A, B, C} end,
    to_unix({Parser(string:slice(Stamp, 0, 10), "-"),       % DATE
             Parser(string:slice(Stamp, 11),    ":")},      % TIME
            Unit);


to_unix({Year, Mon, Day}, Unit) ->
    to_unix({{Year, Mon, Day}, {0, 0, 0}}, Unit);


to_unix(DateTime, Unit) ->
    Secs = calendar:datetime_to_gregorian_seconds(DateTime) - ?SECS_EPOCH,
    scale_unix(Secs, second, Unit).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec get_scale(Unit :: time_unit()) -> dts_1970().
%%
% @doc  Returns the scale factor with respect to seconds.
% @end  --
get_scale(Unit) ->
    maps:get(Unit, #{millisecond => 1000,
                     second      => 1,
                     minute      => ?SECS_IN_MIN,
                     hour        => ?SECS_IN_HOUR,
                     day         => ?SECS_IN_DAY}).

