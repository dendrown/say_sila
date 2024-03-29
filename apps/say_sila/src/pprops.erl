%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Prop-proplists: recursive property list toolbox
%%
%% @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(pprops).

-export([get_split/2,       get_split/3,
         get_first_value/2, get_first_value/3,
         get_value/2,       get_value/3,
         get_values/2]).

-include("types.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec get_split(Key   :: term(),
                Props :: proplist()) -> {term(), proplist()}.
%%
% @doc  Generalized/simplified one-key splitter for property lists.
%
%       NOTE: This function is a candidate for a utility box module.
% @end  --
get_split(Key, Props) ->
    get_split(Key, Props, undefined).



%%--------------------------------------------------------------------
-spec get_split(Key     :: term(),
                Props   :: proplist(),
                Default :: term()) -> {term(), proplist()}.
%%
% @doc  Generalized/simplified one-key splitter for property lists.
%       Caller specifies a `Default' property value, returned when
%       `Props' does not contain `Key'.
% @end  --
get_split(Key, Props, Default) ->

    case proplists:split(Props, [Key]) of
        {[ [            ] ], Rest} -> {Default, Rest};
        {[ [{Key, Value}] ], Rest} -> {Value,   Rest};
        {[ [ Key        ] ], Rest} -> {true,    Rest}
    end.



%%--------------------------------------------------------------------
-spec get_first_value(Keys    :: keys(),
                      List    :: options()) -> undefined|term().

-spec get_first_value(Keys    :: keys(),
                      List    :: options(),
                      Default :: term()) -> term().
%%
% @doc  Returns the value from a two-level property list.  If the list
%       contains no value for a key, the function returns `Default'.
%
% NOTE: An atom as a singleton key or option (rather than a list) will
%       be promoted to list for so that the function works as expected.
% @end  --
get_first_value(Keys, List) ->
    get_first_value(Keys, List, undefined).


get_first_value(Keys, Option, Default) when is_atom(Option) ->
    % Promote a singleton option to an option list
    get_value(Keys, [Option], Default);


get_first_value([], _, Default) ->
    Default;


get_first_value([Key|Rest], List, Default) ->
    case proplist:get_value(Key, List) of
        undefined -> get_first_value(Rest, List, Default);
        Value     -> Value
    end.



%%--------------------------------------------------------------------
-spec get_value(Keys :: keys(),
                List :: options()) -> undefined|term().

-spec get_value(Keys    :: keys(),
                List    :: options(),
                Default :: term()) -> term().
%%
% @doc  Returns a value from a recursive property list, such that:
%       [{`Key1', [{`Key2', `ReturnedValue'}|...]|...].  When a
%       `Default' is specified and the list contains no value for
%       the key sequence, the function returns `Default'. Otherwise,
%       it returns `undefined' when the value is not found.
%
% NOTE: An atom as a singleton key or option (rather than a list) will
%       be promoted to list for so that the function works as expected.
% @end  --
get_value(Keys, List) ->
    get_value(Keys, List, undefined).


get_value(Keys, Option, Default) when is_atom(Option) ->
    % Promote a singleton option to an option list
    get_value(Keys, [Option], Default);


get_value(Key, List, Default) when is_atom(Key) ->
    % Promote a singleton key to a key list
    get_value([Key], List, Default);


get_value([], List, Default) when is_list(List) ->
    Default;


get_value([Key], List, Default) ->
    proplists:get_value(Key, List, Default);


get_value([Key|SubKeys], List, Default) ->
    case proplists:get_value(Key, List) of
        undefined -> Default;
        SubList   -> get_value(SubKeys, SubList, Default)
    end.



%%--------------------------------------------------------------------
-spec get_values(Items :: [term()]
                        | [{term(), term()}],
                 List :: proplist()) -> [undefined|term()].
%%
% @doc  Returns a list of values associated with a lists of keys with
%       respect to a property list.
% @end  --
get_values(Items, List) ->
    Getter = fun
        ({Key, Default}) -> proplists:get_value(Key, List, Default);
        ( Key )          -> proplists:get_value(Key, List)
    end,
    [Getter(I) || I <- Items].



%%====================================================================
%% Internal functions
%%====================================================================
