-module(wol_strings).

-export([template/2,
	 template/3]).

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
    
