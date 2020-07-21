%%%-------------------------------------------------------------------
%%
%%       _/_/_/  _/_/_/  _/_/_/    _/      _/  _/
%%     _/          _/    _/    _/    _/  _/    _/
%%      _/_/      _/    _/_/_/        _/      _/
%%         _/    _/    _/    _/      _/      _/
%%  _/_/_/    _/_/_/  _/_/_/        _/      _/_/_/_/
%%
%% @doc Utility functions for tweets.
%%
%% @copyright 2019 Dennis Drown and Jonathan Hollander
%% @end
%%%-------------------------------------------------------------------
-module(tweet).
-author("Dennis Drown <den@sibyl.guru>").

-export([from_map/1]).

-include("twitter.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec from_map(TwMap :: map()) -> tweet().
%%
% @doc  Converts a raw status message (tweet) in map form to a Sila
%       tweet record.
% @end  --
from_map(Status = #{<<"id_str">>    := ID,
                    <<"lang">>      := Lang,
                    <<"text">>      := Text,
                    <<"user">>      := User,
                    <<"retweeted">> := IsRetweet}) ->

    % TODO: Older tweets, don't have a "timestamp_ms" field, and the calendar
    %       library doesn't support Twitter's format in "created_at".  For now
    %       this isn't critical as we don't need a DTS for the older tweets.
    Millis = maps:get(<<"timestamp_ms">>, Status, undefined),

    %Pull the user information
    #{<<"screen_name">> := UserSN,
      <<"name">>        := UserName,
      <<"description">> := UserDescr} = User,

    % Get information on retweeted status & author
    {Type,
     RetweetID,
     RetweetSN} = case IsRetweet of
        false -> {tweet, undefined, undefined};
        true  ->
            #{<<"id_str">> := RT_ID,
              <<"user">>   := RT_User} = maps:get(<<"retweeted_status">>, Status),

            {retweet, RT_ID, maps:get(<<"screen_name">>, RT_User)}
    end,

    #tweet{id             = ID,
           type           = Type,
           timestamp_ms   = Millis,
           %---------------------------------- % User
           screen_name    = UserSN,
           name           = UserName,
           description    = UserDescr,
           full_text      = Text,
           lang           = Lang,
           %---------------------------------- % Retweeted author
           rt_id          = RetweetID,
           rt_screen_name = RetweetSN}.


%%====================================================================
%% Internal functions
%%====================================================================
