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
         date_str/1,
         datetime_to_unix/2,
         dayize/1,
         earlier/2,
         hourize/1,
         later/2,
         minutize/1,
         str/1,
         sub/3,
         unix_to_datetime/2]).

-include("dts.hrl").

-define(SECS_EPOCH,     62167219200).
-define(SECS_IN_MIN,    60).
-define(SECS_IN_HOUR,  (60 * ?SECS_IN_MIN)).
-define(SECS_IN_DAY,   (24 * ?SECS_IN_HOUR)).



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
-spec date_str(DTS :: tuple()) -> string().
%
%     Creates a printable date from a datetime tuple.
% @end  --
date_str({Date, {_ ,_ ,_}}) ->
    str(Date);


date_str(Date) ->
    str(Date).



%%--------------------------------------------------------------------
-spec datetime_to_unix(DateTime :: datetime()
                                 | date(),
                       Unit     :: atom()) -> non_neg_integer().
%
%     Returns the Epoch timestamp (specify `second' or `millisecond')
%     corresponding to Erlang datetime tuple: `{{year,mon,day},{hour,min,sec}}'
% @end  --
datetime_to_unix({Year, Mon, Day}, Unit) ->
    datetime_to_unix({{Year, Mon, Day}, {0, 0, 0}}, Unit);


datetime_to_unix(DateTime, Unit) ->
    Secs = calendar:datetime_to_gregorian_seconds(DateTime) - ?SECS_EPOCH,
    case Unit of
        millisecond -> Secs * 1000;
        second      -> Secs
    end.



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
-spec str(DTS :: tuple()) -> string().
%
%     Creates an ISO 8601 printable string from a datetime tuple.
% @end  --
str({Year, Mon, Day}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Mon, Day]);


str({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Year, Mon, Day, Hour, Min, Sec]).



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
-spec unix_to_datetime(DTS1970 :: integer(),
                       Unit    :: atom()) -> tuple().
%
%     Returns the Epoch-DTS (specify `second' or `millisecond') as a
%     datetime tuple: `{{year,mon,day},{hour,min,sec}}'
% @end  --
unix_to_datetime(DTS1970, Unit) ->
    add({{1970,1,1},{0,0,0}}, DTS1970, Unit).



%%====================================================================
%% Internal functions
%%====================================================================
