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

-define(GENDER_DIR, "fnode/say/resources/gender/pan").



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec prep_gender(Year :: non_neg_integer()) -> ok.
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
                  DataDir :: binary()) -> ok.
%%
% @doc  Prepares PAN data for the specified Year for analysis and
%       system evaluation.  This involves creating an ARFF which
%       includes tweets from users whose gender we know.
% @end  --
prep_gender(_Year, DataDir) ->
    GenderDir  = ?str_fmt("~s/~s/~s", [code:priv_dir(say_sila), ?GENDER_DIR, DataDir]),
    TruthFpath = ?str_fmt("~s/truth.txt", [GenderDir]),

    % Function to complain if something goes wrong
    LogError  = fun(Tag, Why) ->
        ?error("Cannot ~s truth file: ~p", [Tag, Why]),
        Why
    end,

    % Function to process one line from the truth file
    ReadTruth = fun Recur(In) ->
        case file:read_line(In) of
            {ok, Line} ->
                [UserCode,
                 Gender, _] = string:split(Line, ":::", all),
                genderize(?str_fmt("~s/~s.xml", [GenderDir, UserCode]), Gender),
                % TODO: The rest of the files!
                % TODO: Recur(In);
                ok;

            eof          -> ok;
            {error, Why} -> LogError(read, Why)
        end
    end,

    case file:open(TruthFpath, [read, binary]) of

        {ok, In}     -> ReadTruth(In);
        {error, Why} -> LogError(open, Why)
    end.



%%--------------------------------------------------------------------
-spec genderize(UserFpath :: io_lib:chars(),
                Gender    :: binary()) -> any().    % FIXME: return()
%%
% @doc  Creates an instance as an ARFF line, specifying a user's gender,
%       for each tweet referenced in the specified User file.
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
        case twitter:pull_tweet(ID, return_maps) of

            #{<<"lang">> := <<"en">>} = T ->
                [tweet:from_map(T)|Acc];

            #{<<"lang">> := Lang} ->
                % NOTE: This is happening alot...too often?
                ?warning("Ignoring non-English tweet ~s: lang[~s]", [ID, Lang]),
                Acc;

            Bummer ->
                ?warning("Problem with tweet ~s: ~p", [ID, Bummer]),
                Acc
        end
    end,

    % TODO: We're starting with a few tweets so we don't freak Twitter out as we develop
    {TodoTwIDs,_} = lists:split(8, TwIDs),
    Tweets = lists:foldl(Tweeter, [], TodoTwIDs),
    arff:from_tweets("gender", Tweets).
