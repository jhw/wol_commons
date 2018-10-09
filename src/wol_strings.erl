-module(wol_strings).

-export([template/2,
	 template/3,
	 join/2,
	 join/3]).

template(Pattern, Args) ->
    template(Pattern, Args, []).

template(Pattern, Args, Options) when is_binary(Pattern) ->
    template(binary_to_list(Pattern), Args, Options);
template(Pattern, Args, {_, list}) ->
    lists:flatten(io_lib:fwrite(Pattern, Args));
template(Pattern, Args, none) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Pattern, Args)));
template(Pattern, Args, Options) ->
    template(Pattern, Args, proplists:lookup(return, Options)).
    
join(Names, Delimiter) ->
    join(Names, Delimiter, []).

join(Names, Delimiter, Options) when is_binary(Delimiter) ->
    join(Names, binary_to_list(Delimiter), Options);
join(Names, Delimiter, {_, list}) ->
    string:join([token(Name) || Name <- Names], Delimiter);
join(Names, Delimiter, none) ->
    list_to_binary(string:join([token(Name) || Name <- Names], Delimiter));
join(Names, Delimiter, Options) ->
    join(Names, Delimiter, proplists:lookup(return, Options)).

token(Value) when is_binary(Value) ->
    binary_to_list(Value);
token(Value) ->
    Value.

