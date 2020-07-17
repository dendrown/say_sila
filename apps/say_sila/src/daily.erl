%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Say-Sila functionality for analyses that model using a
%%      day-by-day period.
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(daily).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([extract_period/2,
         step/2, step/3,
         tomorrow/1]).

-import(proplists, [get_value/2, get_value/3]).

-include("sila.hrl").
-include("dts.hrl").
-include("types.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec extract_period(Tracker :: atom(),
                     Options :: list()) -> {date() | datetime(),
                                            date() | datetime(),
                                            list()}.
%%
% @doc  Creates the start/stop period for tweet analysis from an `Options'
%       proplist.  The period defaults are:
%           - `start'   the DTS of the earliest tweet for the `Tracker' code
%           - `stop'    the DTS for midnight tomorrow morning
%                       (so that we pull up to the end of today)
%
%       The caller gets back a tuple with the period start and stop dates
%       and the `Options' proplists with the start/stop elements removed.
% @end  --
extract_period(Tracker, Options) ->

    PeriodStart = case proplists:get_value(start, Options) of
        undefined -> twitter:get_first_dts(Tracker, [calendar]);
        StartDTS  -> StartDTS
    end,

    PeriodStop = case proplists:get_value(stop, Options) of
        undefined -> dts:add(calendar:local_time(), 1, day);    % Tomorrow for today
        StopDTS   -> StopDTS
    end,

    % For a clean day-to-day progression, start/end everything at midnight
    {dts:dayize(PeriodStart),
     dts:dayize(PeriodStop),
     proplists:delete(start, proplists:delete(stop, Options))}.



%%--------------------------------------------------------------------
-spec step(Today       :: datetime(),
           StopDay     :: datetime()) -> {datetime(), proplist()}
                                       | stop.

-spec step(Today       :: datetime(),
           StopDay     :: datetime(),
           Options     :: list()) -> {datetime(), proplist()}
                                   | stop.
%%
% @doc  Increments the daily start/stop options, returning a pair with
%       the datetime corresponding to the next day and options for the
%       current processing step (all options for step/3).  This behaviour
%       continues until daily processing hits the stop date at which point
%       the function returns `stop'.
% @end  --
step(Today, StopDay) ->
    step(Today, StopDay, []).


step(Today, StopDay, Options) ->

    case Today < StopDay of
        true ->
            Tomorrow = tomorrow(Today),
            {Tomorrow, [{start, Today},
                        {stop,  Tomorrow} | Options]};
        false -> stop
    end.



%%--------------------------------------------------------------------
-spec tomorrow(Today :: datetime()) -> datetime().
%%
% @doc  Given a datetime, returns the datetime corresponding to the
%       following day.
% @end  --
tomorrow(Today) ->
    dts:add(Today, 1, day).



%%====================================================================
%% Internal functions
%%====================================================================
