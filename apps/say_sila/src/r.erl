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
         eval/2,
        report_emotions/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("raven.hrl").


-define(BIG_TAG,    <<"BIG">>).
-define(BIG_COLOUR, <<"red">>).

-define(REG_TAG,    <<"REG">>).
-define(REG_COLOUR, <<"blue">>).


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
    %
    ?debug("CMD: ~s", [Cmd]),
    Rsp = eri:eval(Cmd),
    %
    ?debug("RSP: ~p", [Rsp]),
    Rsp;


eval(_) ->
    % TODO: Sending commands as binaries will freeze the eri process.
    %       Try to make the interface more client-friendly.
    ?warning("R commands must be in string (list) form!").



%%--------------------------------------------------------------------
-spec eval(Fmt  :: string(),
           Args :: list()) -> term().
%%
% @doc  Accept R commands in io:format style and evaluate them.
% @end  --
eval(Fmt, Args) ->
    Cmd = io_lib:format(Fmt, Args),
    eval(lists:flatten(Cmd)).



%%--------------------------------------------------------------------
-spec report_emotions(Tag     :: string(),
                      Period  :: atom(),
                      Reports :: map()) -> ok.
%%
% @doc  Create reports for hourly or daily (soon) emotion totals.
% @end  --
report_emotions(Tag, Period, Reports) ->
    gen_server:cast(?MODULE, {report_emotions, Tag, Period, Reports}).




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
            {ok, #state{eri_status = true}};
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


handle_cast({report_emotions, Tag, Period, #{big := BigRpt,
                                             reg := RegRpt}},
            State) ->
    % Start with the earliest data and go to the latest,
    % but drop the first and last period as they are partials
    BegDTS = dts:earlier(dts:add(BigRpt#report.beg_dts, 1, Period),
                         dts:add(RegRpt#report.beg_dts, 1, Period)),
    EndDTS = dts:later(dts:sub(BigRpt#report.end_dts, 1, Period),
                       dts:sub(RegRpt#report.end_dts, 1, Period)),
    ?info("Running report from ~s to ~s: per[~s]", [dts:str(BegDTS),
                                                    dts:str(EndDTS),
                                                    Period]),
    plot_emotions(Tag,
                  Period,
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
-spec plot_emotions(Tag     :: string(),
                    Period  :: atom(),
                    BegDTS  :: tuple(),
                    EndDTS  :: tuple(),
                    BigEmos :: map(),
                    RegEmos :: map()) -> ok.
%%
% @doc  Prepare Sila report and plot it in R.
% @end  --
plot_emotions(Tag, Period, BegDTS, EndDTS, BigEmos, RegEmos) ->
    GraphDir = wui:get_graph_dir(),
    EmoFiler = fun(Emotion) ->
                   FStub    = io_lib:format("~s.~s.~s", [Tag, Period, Emotion]),
                   FPathCSV = io_lib:format("~s/R/~s.csv", [?WORK_DIR, FStub]),
                   FPathPNG = io_lib:format("~s/~s.png", [GraphDir, FStub]),
                   %
                   % We've got data and plot filenames now; but just open the data file
                   ok = filelib:ensure_dir(FPathCSV),
                   {ok, FOut} = file:open(FPathCSV, [write]),
                   io:format(FOut, "~s,~s~n", [?BIG_TAG, ?REG_TAG]),
                   ?info("Creating data file: ~s", FPathCSV),
                   #emo_file{fpath_csv = FPathCSV,
                             fpath_png = FPathPNG,
                             io        = FOut}
                   end,
    %
    % NOTE: There is a bit of flux as to the best way to handle four simultaneous files.
    EmoOuts = #emo_files{anger   = EmoFiler(anger),
                         fear    = EmoFiler(fear),
                         sadness = EmoFiler(sadness),
                         joy     = EmoFiler(joy)},
    fill_emotion_files(Period, BegDTS, EndDTS, BigEmos, RegEmos, EmoOuts),
    RptFiles = lists:map(fun({Emo, Ndx}) ->
                             EmoFile = element(Ndx, EmoOuts),
                             {Emo, EmoFile}
                             end,
                         lists:zip(record_info(fields, emo_files),
                                   lists:seq(2, record_info(size, emo_files)))),
    lists:foreach(fun(Rpt) -> graph_emotion(Period, Rpt) end,
                  RptFiles).



%%--------------------------------------------------------------------
-spec fill_emotion_files(Period  :: atom(),
                         CurrDTS :: tuple(),
                         EndDTS  :: tuple(),
                         BigEmos :: map(),
                         RegEmos :: map(),
                         EmoOuts :: emo_files()) -> [string()].
%%
% @doc  Prepare Sila report and plot it in R.
% @end  --
fill_emotion_files(_, CurrDTS, EndDTS, _, _, EmoOuts) when CurrDTS > EndDTS ->
    lists:foreach(fun(Ndx) ->
                      Out = element(Ndx, EmoOuts),
                      file:close(Out#emo_file.io)
                      end,
                  lists:seq(2, record_info(size, emo_files)));


fill_emotion_files(Period, CurrDTS, EndDTS, BigEmos, RegEmos, EmoOuts) ->
    ?debug("Processing report for ~s", [dts:str(CurrDTS)]),
    BigEmo = maps:get(CurrDTS, BigEmos, #emotions{}),
    RegEmo = maps:get(CurrDTS, RegEmos, #emotions{}),
    lists:foreach(fun(Ndx) ->
                      Out = element(Ndx, EmoOuts),
                      io:format(Out#emo_file.io, "~.6f,~.6f~n",
                               [element(Ndx, BigEmo),
                                element(Ndx, RegEmo)])
                      end,
                  lists:seq(2, record_info(size, emo_files))),
    fill_emotion_files(Period, dts:add(CurrDTS, 1, Period), EndDTS, BigEmos, RegEmos, EmoOuts).




%%--------------------------------------------------------------------
-spec graph_emotion(Period  :: atom(),
                    EmoFile :: {atom(), emo_file()}) -> ok.
%%
% @doc  Plot the emotion data in R and collect the image file.
% @end  --
graph_emotion(Period,
              {Emotion, #emo_file{fpath_csv = FPathCSV,
                                  fpath_png = FPathPNG}}) ->
    %
    ?info("Creating ~s graph: ~s", [Emotion, FPathPNG]),
    Title = io_lib:format("~s in tweets: ~s vs. ~s players", [Emotion, ?BIG_TAG, ?REG_TAG]),
    eval("png(file = '~s')", [FPathPNG]),
    eval("data <- read.csv('~s')", [FPathCSV]),
    eval("yRng <- signif(10 + range(data$BIG, data$REG), digits=2)"),
    eval("plot(data$~s, type='l', col='~s', ylim=yRng, xlab='~ss', ylab='~s', main='~s')", [?BIG_TAG,
                                                                                            ?BIG_COLOUR,
                                                                                            Period,
                                                                                            Emotion,
                                                                                            Title]),
    eval("lines(data$~s, type='l', col='~s')", [?REG_TAG, ?REG_COLOUR]),
    eval("legend(1, yRng[2], c('~s','~s'), cex=0.8, col=c('~s','~s'), pch=21:22, lty=1:2)", [?BIG_TAG,
                                                                                             ?REG_TAG,
                                                                                             ?BIG_COLOUR,
                                                                                             ?REG_COLOUR]),
    eval("dev.off()").
