%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Data collection/preparation from PAN: https://pan.webis.de/
%%
%% @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(pan).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([prep_gender/1]).


-include("sila.hrl").
-include("ioo.hrl").
-include("twitter.hrl").
-include("types.hrl").
-include_lib("llog/include/llog.hrl").

-define(GENDER_DIR,  <<"fnode/say/resources/gender/pan">>).
-define(GENDER_TAG,  <<"gender">>).



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec prep_gender(Year :: non_neg_integer()) -> term().
%%
% @doc  Prepares PAN data for the specified Year for analysis and
%       system evaluation.  This involves creating an ARFF which
%       includes tweets from users whose gender we know.
% @end  --
prep_gender(Year) ->
    DataDir = case Year of
        2014 -> <<"pan14-author-profiling-training-corpus-english-twitter-2014-04-16">>
    end,
    ?info("PAN-~B: ~s", [Year, DataDir]),
    prep_gender(Year, DataDir).



%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec prep_gender(Year    :: non_neg_integer(),
                  DataDir :: binary()) -> term().
%%
% @doc  Prepares PAN data for the specified Year for analysis and
%       system evaluation.  This involves creating an ARFF which
%       includes tweets from users whose gender we know.
% @end  --
prep_gender(Year, DataDir) ->
    GenderInfo = [{target, gender}],
    GenderARFF = ?str_fmt("~s.pan~B", [?GENDER_TAG, Year]),
    GenderDir  = ?str_fmt("~s/~s/~s", [code:priv_dir(say_sila), ?GENDER_DIR, DataDir]),
    TruthFpath = ?str_fmt("~s/truth.txt", [GenderDir]),

    % Setup the ARFF for tweets+gender
    FOutCont = {cont, FOut} = arff:from_tweets(GenderARFF, [], [{mode, start} | GenderInfo]),
    FOutInfo = [{mode, FOutCont} | GenderInfo],

    % Function to complain if something goes wrong
    LogError  = fun(Tag, Why) ->
        ?error("Cannot ~s truth file: ~p", [Tag, Why]),
        Why
    end,

    % Function to process one line from the truth file
    ReadTruth = fun Recur(In) ->
        case file:read_line(In) of
            {ok, Line} ->
                [UserCode, Gender, _] = string:split(Line, ":::", all),

                Fpath  = ?str_fmt("~s/~s.xml", [GenderDir, UserCode]),
                Tweets = genderize(Fpath, Gender),
                arff:from_tweets(GenderARFF, Tweets, FOutInfo),
                Recur(In);

            eof          -> ok;
            {error, Why} -> LogError(read, Why)
        end
    end,

    % Process all the users in the truth file
    case file:open(TruthFpath, [read, binary]) of

        {ok, In} ->
            ReadTruth(In),
            file:close(In);

        {error, Why} -> LogError(open, Why)
    end,

    arff:from_tweets(GenderARFF, [], [{mode, {stop, FOut}}]).



%%--------------------------------------------------------------------
-spec genderize(UserFpath :: io_lib:chars(),
                Gender    :: binary()) -> [map()].
%%
% @doc  Returns a list of all tweets referenced in the specified User
%       file with the Gender value added to each tweet map.
% @end  --
genderize(UserFpath, Gender) ->

    % Function to extract the tweet IDs out of a single XML
    GetTwIDs = fun

        ({startElement,_,"document",_,[_,{attribute,"id",_,_,ID}]}, {C,IDs}) ->
            %?info("TwID: ~p", [ID]),
            {C+1, [ID|IDs]};

        (_Evt, Acc) ->
            %?debug("XML: ~p", [_Evt]),
            Acc
    end,

    % The file they sent is an XML document
    {Cnt, TwIDs} = case file:read_file(UserFpath) of
        {ok, UserXML} ->
            {ok, Acc, _} = erlsom:parse_sax(UserXML, {0,[]}, GetTwIDs),
            Acc;
        {error, Why}  ->
            ?error("Cannot read file ~s: ~p", [UserFpath, Why]),
            []
    end,

    ?debug("Parsed ~s ( ~-6s ) cnt[~4B]", [lists:nth(2, string:split(UserFpath, "/", trailing)),
                                           Gender, Cnt]),

    % Function to pull the tweet and convert to ARFF-ready data
    Tweeter = fun(ID, Acc) ->
        % We should be able to pull 900 tweets every 15 minute window (3600/hr or 1/sec)
        % We'll go at half that rate in case other nodes are doing doing something too.
        % @ref https://developer.twitter.com/en/docs/basics/rate-limits.html
        timer:sleep(2000),
        case twitter:pull_tweet(ID, return_maps) of

            #{<<"lang">> := <<"en">>} = T ->
                Tweet = maps:put(gender, Gender, T),
                [Tweet|Acc];

            #{<<"lang">> := Lang} ->
                % NOTE: This is happening alot...too often?
                ?warning("Ignoring non-English tweet ~s: lang[~s]", [ID, Lang]),
                Acc;

            Bummer ->
                ?warning("Problem with tweet ~s: ~p", [ID, Bummer]),
                Acc
        end
    end,

    % Run that Tweet ID list by the Twitter API
    TodoTwIDs = TwIDs,                      % DEV:{TodoTwIDs,_} = lists:split(2, TwIDs),
    lists:foldl(Tweeter, [], TodoTwIDs).
