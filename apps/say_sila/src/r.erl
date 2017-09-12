%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc R<==>Erlang bridge for Sila
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(r).
-behaviour(gen_server).

-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([start_link/0,
         stop/0,
         eval/1,
        report_emotions/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("raven.hrl").


-record(state, {eri_status :: term() }).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                    |  ignore
                    |  {error, term()}.
%%
% @doc  Startup function for Twitter services
% @end  --
start_link() ->
    gen_server:start_link({?REG_DIST, ?MODULE}, ?MODULE, [go], []).



%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for Twitter services
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).




%%--------------------------------------------------------------------
-spec eval(Cmd :: string()) -> term().
%%
% @doc  Pass-thru evaluator for rErlang.
% @end  --
eval(Cmd) when is_list(Cmd) ->
    eri:eval(Cmd);

eval(_) ->
    % TODO: Sending commands as binaries will freeze the eri process.
    %       Make the interface more client-friendly.
    ?warning("R commands must be in string (list) form!").



%%--------------------------------------------------------------------
-spec report_emotions(Tracker :: atom(),
                      Period  :: atom(),
                      Reports :: map()) -> ok.
%%
% @doc  Create reports for hourly or daily (soon) emotion totals.
% @end  --
report_emotions(Tracker, Period, Reports) ->
    gen_server:cast(?MODULE, {report_emotions, Tracker, Period, Reports}).




%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(list()) -> any().
%%
% @doc  Initiates connection with R environment
% @end  --
init([go]) ->
    ?notice("Initializing R/Elang bridge"),
    process_flag(trap_exit, true),

    Return = case eri:start() of
        true ->
            gen_server:cast(self(), connect),
            {ok, #state{}};
        Fail ->
            {stop, Fail}
    end,
    Return.



%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: state()) -> normal.
%%
% @doc  Server shutdown callback.
% @end  --
terminate(Why, _State) ->
    ?notice("Shutting down the R/Erlang bridge: why[~p]", [Why]),
    eri:stop(),
    normal.


%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(),
                  State  :: state(),
                  Extra  :: term()) -> {atom(), term()}.
%%
% @doc  Hot code update processing: a placeholder.
% @end  --
code_change(OldVsn, State, _Extra) ->
    ?notice("Hot code update: old[~p]", [OldVsn]),
    {ok, State}.


%%--------------------------------------------------------------------
%% handle_call:
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% handle_cast
%%
% @doc  Process async messages
% @end  --
handle_cast(connect, State) ->
    Eri = eri:connect(),
    Action = case Eri of
        {ok, 0} ->
            ?info("Connected to R environment"),
            noreply;
        Fail ->
            ?info("Failed to connect to R environment: why[~p]", [Fail]),
            stop
    end,
    {Action, State#state{eri_status = Eri}};


handle_cast({report_emotions, Tracker, hourly, #{big := BigRpt,
                                                 reg := RegRpt}},
            State) ->
    % TODO: generalizing the reporting and use a date/time formatting library
    %
    % Start with the earliest data and go to the latest,
    % but drop the first and last hour as they are partials
    BegDTS = dts:earlier(dts:add(BigRpt#report.beg_dts, 1, hour),
                         dts:add(RegRpt#report.beg_dts, 1, hour)),
    EndDTS = dts:later(dts:sub(BigRpt#report.end_dts, 1, hour),
                       dts:sub(RegRpt#report.end_dts, 1, hour)),
    ?info("Running hourly report from ~s to ~s", [dts:str(BegDTS),
                                                  dts:str(EndDTS)]),
    plot_emotions(Tracker,
                  BegDTS,
                  EndDTS,
                  BigRpt#report.emotions,
                  RegRpt#report.emotions),
    {noreply, State};


handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% handle_info:
%%
% @doc  Process out-of-band messages
% @end  --
handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec plot_emotions(Tracker :: atom(),
                    BegDTS  :: tuple(),
                    EndDTS  :: tuple(),
                    BigEmos :: map(),
                    RegEmos :: map()) -> string().
%%
% @doc  Prepare Sila report and plot it in R.
% @end  --
plot_emotions(Tracker, BegDTS, EndDTS, BigEmos, RegEmos) ->
    EmoFiler = fun(Emotion) ->
                   FPath = lists:flatten(io_lib:format("~s/R/~s.~s.csv", [?WORK_DIR, Emotion, Tracker])),
                   ok = filelib:ensure_dir(FPath),
                   {ok, FOut} = file:open(FPath, [write]),
                   #emo_file{fpath = FPath,
                             io    = FOut}
                   end,
    %
    % NOTE: There is a bit of flux as to the best way to handle four simultaneous files.
    EmoOuts = #emo_files{anger   = EmoFiler(anger),
                         fear    = EmoFiler(fear),
                         sadness = EmoFiler(sadess),
                         joy     = EmoFiler(joy)},
    fill_emotion_files(BegDTS, EndDTS, BigEmos, RegEmos, EmoOuts),
    lists:map(fun({Emo, Ndx}) ->
                  EmoFile = element(Ndx, EmoOuts),
                  {Emo, EmoFile#emo_file.fpath}
                  end,
              lists:zip(record_info(fields, emo_files),
                        lists:seq(2, record_info(size, emo_files)))).



%%--------------------------------------------------------------------
-spec fill_emotion_files(CurrDTS :: tuple(),
                         EndDTS  :: tuple(),
                         BigEmos :: map(),
                         RegEmos :: map(),
                         EmoOuts :: emo_files()) -> [string()].
%%
% @doc  Prepare Sila report and plot it in R.
% @end  --
fill_emotion_files(CurrDTS, EndDTS, _, _, EmoOuts) when CurrDTS > EndDTS ->
    lists:foreach(fun(Ndx) ->
                      Out = element(Ndx, EmoOuts),
                      file:close(Out#emo_file.io)
                      end,
                  lists:seq(2, record_info(size, emo_files)));


fill_emotion_files(CurrDTS, EndDTS, BigEmos, RegEmos, EmoOuts) ->
    BigEmo = maps:get(CurrDTS, BigEmos, #emotions{}),
    RegEmo = maps:get(CurrDTS, RegEmos, #emotions{}),
    lists:foreach(fun(Ndx) ->
                      Out = element(Ndx, EmoOuts),
                      io:format(Out#emo_file.io, "~.6f,~.6f~n",
                               [element(Ndx, BigEmo),
                                element(Ndx, RegEmo)])
                      end,
                  lists:seq(2, record_info(size, emo_files))),
    fill_emotion_files(dts:add(CurrDTS, 1, hour), EndDTS, BigEmos, RegEmos, EmoOuts).
