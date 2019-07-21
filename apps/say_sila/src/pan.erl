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
                Recur(In);

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
                Gender    :: binary()) -> ok.
%%
% @doc  Creates an instance as an ARFF line, specifying a user's gender,
%       for each tweet referenced in the specified User file.
% @end  --
genderize(UserFpath, Gender) ->

    ?debug("Processing ~s: gnd[~s]", [UserFpath, Gender]),
    GetTwIDs = fun
        ({startElement,_,"document",_,[_,{attribute,"id",_,_,TwID}]}, Acc) ->
            [TwID|Acc];
        (_Evt, Acc) ->
            %?debug("XML: ~p", [_Evt]),
            Acc
    end,

    TwIDs = case file:read_file(UserFpath) of
        {ok, UserXML} ->
            {ok, IDs, _} = erlsom:parse_sax(UserXML, [], GetTwIDs),
            IDs;
        {error, Why}  ->
            ?error("Cannot read file ~s: ~p", [UserFpath, Why]),
            []
    end,

    ?info("Tweets: ~p", [TwIDs]).

