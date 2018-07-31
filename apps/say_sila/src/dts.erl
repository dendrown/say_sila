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
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(dts).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([add/3,
         date_str/1, date_str/2,
         date_STR/1, date_STR/2,
         dayize/1,
         dayize/2,
         earlier/2,
         hourize/1,
         later/2,
         minutize/1,
         minutize/2,
         quarter/1,
         quarter/2,
         str/1,
         str/2,
         sub/3,
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
%     Adds the specified amout of `second', `minute', `hour' or `day'
%     to DTS.
% @end  --
add(Day = {_, _, _}, Amt, Unit) ->
    add({Day, {0, 0, 0}}, Amt, Unit);


add(DTS, Amt, Unit) ->
    Secs = case Unit of
        millisecond -> Amt div 1000;
        second      -> Amt;
        minute      -> Amt * ?SECS_IN_MIN;
        hour        -> Amt * ?SECS_IN_HOUR;
        day         -> Amt * ?SECS_IN_DAY
    end,
    OrigSecs = calendar:datetime_to_gregorian_seconds(DTS),
    NewSecs  = OrigSecs + Secs,
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
    case Unit of
        millisecond -> Secs * 1000;
        second      -> Secs
    end.



%%====================================================================
%% Internal functions
%%====================================================================
