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

-export([earlier/2,
         hourize/1,
         later/2,
         minutize/1,
         unix_to_datetime/2]).



%%====================================================================
%% API
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
-spec unix_to_datetime(DTS1970 :: integer(),
                       Unit    :: atom()) -> tuple().
%
%     Returns the Epoch-DTS (specify `second' or `millisecond') as a
%     datetime tuple: `{{year,mon,day},{hour,min,sec}}'
% @end  --
unix_to_datetime(DTS1970, Unit) ->
    Secs1970 = case Unit of
        millisecond -> DTS1970 div 1000;
        second      -> DTS1970
    end,
    Base = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Secs = Base + Secs1970,
    calendar:gregorian_seconds_to_datetime(Secs).



%%====================================================================
%% Internal functions
%%====================================================================
