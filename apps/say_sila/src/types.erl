%%%-------------------------------------------------------------------
%%
%%        _/_/_/  _/_/_/  _/          _/_/
%%     _/          _/    _/        _/    _/
%%      _/_/      _/    _/        _/_/_/_/
%%         _/    _/    _/        _/    _/
%%  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
%%
%% @doc Type toolbox
%%
%% @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
%% @end
%%%-------------------------------------------------------------------
-module(types).
-author("Dennis Drown <drown.dennis@courrier.uqam.ca>").

-export([restring/1,    reSTRING/1,
         join/2,        join/3,
         split_atom/1,
         to_atom/1,     to_atom/2,      to_atom/3,
         to_binary/1,   to_BINARY/1,
         to_float/1]).

-include("types.hrl").
-include("ioo.hrl").

%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec restring(List :: [stringy()]) -> string().
%%
% @doc  Takes an io_lib stringy list which allows atoms, and converts
%       the atoms to strings.
% @end  --
restring(List) ->
    Stringify = fun(X) ->
        if  is_atom(X) -> atom_to_list(X);
            true       -> X
        end
    end,
    [Stringify(X) || X <- List].



%%--------------------------------------------------------------------
-spec reSTRING(List :: stringy()) -> string().
%%
% @doc  Takes an io_lib stringy list which allows atoms, and converts
%       the atoms to strings.  The function flattens the returned string.
% @end  --
reSTRING(List) ->
    lists:flatten(restring(List)).



%%--------------------------------------------------------------------
-spec join(Delim :: string(),
           List  :: [stringy()]) -> string().

-spec join(Delim :: string(),
           List  :: [stringy()],
           Fun   :: fun((stringy()) -> string())) -> string().
%%
% @doc  Returns a string with the elements of List separated by `Delim'.
%       This function works as lists:join/2 except that it converts
%       atoms to strings to ease your I/O woes.
%
%       `join/3' applies `Fun' to each element in the list before
%       it joins the elements.
% @end  --
join(Delim, List) ->
    lists:join(Delim, restring(List)).


join(Delim, List, Fun) ->
    lists:join(Delim, lists:map(Fun, restring(List))).



%%--------------------------------------------------------------------
-spec split_atom(Atom :: stringy()) -> [string()].
%%
% @doc  Converts an atom into a list of part strings separated by
%       underscores in the original atom.
% @end  --
split_atom(Atom) ->
    string:split(atom_to_list(Atom), "_").



%%--------------------------------------------------------------------
-spec to_atom(Value :: stringable()) -> atom().
%%
% @doc  Changes basic string forms to (pre-existing) atoms.
% @end  --
to_atom(Value) when is_binary(Value) -> binary_to_existing_atom(Value, utf8);
to_atom(Value) when is_list(Value)   -> list_to_existing_atom(Value);
to_atom(Value) when is_atom(Value)   -> Value.



%%--------------------------------------------------------------------
-spec to_atom(Prefix :: stringable(),
              Value  :: stringable()
                      | integer()) -> atom().
%%
% @doc  Adds `Value' to the specified `Prefix' and returns the result
%       as an (existing) atom.
%
%       Alternate usage: if `Value' is the atom `create', then the
%       function works as `to_atom/1', creating an atom out of the
%       first argument, except that the atom need not exist already.
% @end  --
to_atom(Value, create) when is_binary(Value) -> binary_to_atom(Value, utf8);
to_atom(Value, create) when is_list(Value)   -> list_to_atom(Value);
to_atom(Value, create) when is_atom(Value)   -> Value;
to_atom(Value, create) when is_atom(Value)   -> Value;


to_atom(Prefix, Value) when is_integer(Value) ->
    to_atom(Prefix, integer_to_list(Value));


to_atom(Prefix, Value) ->

    try list_to_existing_atom(?str_FMT("~s_~s", [Prefix, Value])) of
        Atom -> Atom
    catch
        error:badarg -> undefined
    end.



%%--------------------------------------------------------------------
-spec to_atom(Prefix :: stringable(),
              Value  :: stringable()
                      | integer(),
              Flag   :: create) -> atom().
%%
% @doc  Returns Prefix_Value as an atom.  If `Flag' is set to `create',
%       then the returned atom need not exist already.
% @end  --
to_atom(Prefix, Value, create) when is_integer(Value) ->
    to_atom(Prefix, integer_to_list(Value), create);


to_atom(Prefix, Value, create) ->
    list_to_atom(?str_FMT("~s_~s", [Prefix, Value])).



%%--------------------------------------------------------------------
-spec to_binary(Value :: number()
                       | stringable()) -> binary().
%%
% @doc  Converts a numeric or stringable value to a binary.
% @end  --
to_binary(Value) when is_atom(Value)    -> atom_to_binary(Value, utf8);
to_binary(Value) when is_list(Value)    -> list_to_binary(Value);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_float(Value)   -> float_to_binary(Value);
to_binary(Value) when is_binary(Value)  -> Value.



%%--------------------------------------------------------------------
-spec to_BINARY(Value :: stringable()) -> binary().
%%
% @doc  Converts a stringable value to a binary in upper case.
% @end  --
to_BINARY(Value) ->
    string:uppercase(to_binary(Value)).



%%--------------------------------------------------------------------
-spec to_float(Value :: number()
                      | stringable()) -> float().
%%
% @doc  Converts an integer/float in string format to a float value.
%       As a courtesy, if we are passed a numeric value, we return it
%       ensuring a float format.
% @end  --
to_float(Value) when is_number(Value) ->
    float(Value);


to_float(Value) ->
     PreConvert = case string:find(Value, <<".">>) of
        nomatch -> [Value, <<".0">>];               % Handle "99"
        _       -> [Value, <<"0">>]                 % Handle "99."
    end,
    {PostConvert, _} = string:to_float(PreConvert),
    PostConvert.




%%====================================================================
%% Internal functions
%%====================================================================
