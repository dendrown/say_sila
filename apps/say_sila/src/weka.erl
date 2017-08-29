%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Sila Weka functionality
%%
%%      Weka runs on the JVM and Sila uses a Clojure/OTP node to
%%      communicate with it.  This module contains the utilities to
%%      prepare for handoffs to Weka.
%%
%%      ARFF formatting corresponds to the requirements of the package
%%      AffectiveTweets, a plugin for Weka:
%%
%%      @see https://github.com/felipebravom/AffectiveTweets
%%
%% @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(weka).

-export([tweets_to_arff/2]).

-include("sila.hrl").
-include("llog.hrl").
-include("twitter.hrl").



%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec tweets_to_arff(Name   :: string(),
                     Tweets :: [tweet()]) -> ok.
%%
% @doc  Convert a list of tweets to an ARFF (Attribute-Relation File Format)
%       file for Weka.
%
%       Sample Weka target from https://github.com/felipebravom/EmoInt :
%
%       java -Xmx4G -cp $HOME/weka-3-8-1/weka.jar weka.Run weka.classifiers.meta.FilteredClassifier
%           -t data/anger-ratings-0to1.train.arff
%           -T data/anger-ratings-0to1.test.target.arff
%           -classifications "weka.classifiers.evaluation.output.prediction.CSV
%               -use-tab
%               -p first-last
%               -file data/anger-pred.csv"
%           -F "weka.filters.MultiFilter
%               -F \"weka.filters.unsupervised.attribute.TweetToEmbeddingsFeatureVector
%                   -I 2
%                   -B $HOME/wekafiles/packages/AffectiveTweets/resources/w2v.twitter.edinburgh.100d.csv.gz
%                   -S 0 -K 15 -L -O\"
%               -F \"weka.filters.unsupervised.attribute.TweetToLexiconFeatureVector
%                   -I 2 -A -D -F -H -J -L -N -P -Q -R -T -U -O\"
%               -F \"weka.filters.unsupervised.attribute.TweetToSentiStrengthFeatureVector
%                   -I 2 -U -O\"
%               -F \"weka.filters.unsupervised.attribute.Reorder
%                   -R 5-last,4\""
%           -W weka.classifiers.functions.LibLINEAR -- -S 12 -C 1.0 -E 0.001 -B 1.0 -L 0.1 -I 1000
%
%   NEED ATTRS: id, screen_name, text
%
% @end  --
tweets_to_arff(Name, Tweets) ->
    FPath = make_fpath(Name),
    {ok, FOut} = file:open(FPath, [write]),

    io:format(FOut, "@RELATION  ~s~n~n", [Name]),
    io:put_chars(FOut, "@ATTRIBUTE id string\n"),
    io:put_chars(FOut, "@ATTRIBUTE screen_name string\n"),
    io:put_chars(FOut, "@ATTRIBUTE text string\n"),

    % Fill in the tweet text as attribute data
    io:put_chars(FOut, "\n@DATA\n"),
    write_tweets(FOut, Tweets),

    % All ready for some learnin'
    FStatus = file:close(FOut),
    ?info("ARFF<create>: path[~s] stat[~p]", [FPath, FStatus]),
    FStatus.




%%====================================================================
%% Internal functions
%%--------------------------------------------------------------------
-spec make_fpath(Name :: string()) -> string().
%%
% @doc  Returns the full filepath for an ARFF file with the specified
%       name.  This function also makes sure the subdirectory it is
%       using under `/tmp' exists.
% @end  --
make_fpath(Name) ->
    FPath = io_lib:format("~s/weka/~s.arff", [?WORK_DIR, Name]),
    ok  = filelib:ensure_dir(FPath),
    FPath.



%%--------------------------------------------------------------------
-spec write_tweets(Out    :: pid(),
                   Tweets :: [[tweet()]]) -> ok.
%%
% @doc  Write out a list of tweets to an ARFF file.
% @end  --
write_tweets(_, []) ->
    ok;

write_tweets(Out, [Tweet = #tweet{text = Text0} | Rest]) ->
    %
    % So Weka doesn't freak:
    Text1 = re:replace(Text0, "'", [$\\, $\\, $'], [global]),               % Escape single quotes
    Text  = re:replace(Text1, "[\r\n]", " ", [global, {return, binary}]),   % Linefeeds/newlines to spaces
    io:format(Out, "'~s','~s','~s'~n", [Tweet#tweet.id,
                                        Tweet#tweet.screen_name,
                                        Text]),
    write_tweets(Out, Rest).

