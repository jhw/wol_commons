-module(wol_registry).

-import(wol_strings, [template/3]).

-export([registry/2]).

registry(LeagueName, Mod) when is_binary(LeagueName) ->
    list_to_atom(template("~s~~~s", [Mod, LeagueName], [{return, list}]));
registry(League, Mod) ->
    registry(maps:get(<<"name">>, League), Mod).
