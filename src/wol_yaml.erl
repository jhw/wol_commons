-module(wol_yaml).

-export([deyamerlise/1,
	 test/0]).

%% converts yamerl:decode/1 format (proplists, lists) to jsx- equivalent format (maps, binaries)

deyamerlise(Struct) ->
    deyamerlise(Struct, []).

deyamerlise([], Results) when is_list(Results) ->
    lists:reverse(Results);
deyamerlise([], Results) ->
    Results;
deyamerlise([{_, _}|_]=Items, []) ->
    deyamerlise(Items, #{});
deyamerlise([{Key, Item}|Items], Results) when is_list(Item) ->
    case is_string(Item) of
	true ->
	    deyamerlise(Items, Results#{format(Key) => format(Item)});
	false ->
	    deyamerlise(Items, Results#{format(Key) => deyamerlise(Item, [])})
    end;
deyamerlise([{Key, Item}|Items], Results) ->
    deyamerlise(Items, Results#{format(Key) => format(Item)});
deyamerlise([Item|Items], Results)  ->
    case is_string(Item) of
	true ->
	    deyamerlise(Items, [format(Item)|Results]);
	false ->
	    deyamerlise(Items, [deyamerlise(Item, [])|Results])
    end.

format(Item) ->
    format(Item, is_string(Item)).

format(Item, true) ->
    list_to_binary(Item);
format(Item, false) ->
    Item.

%% https://stackoverflow.com/questions/2479713/determining-if-an-item-is-a-string-or-a-list-in-erlang

is_string([]) -> 
    true;
is_string([X|T]) -> 
    is_integer(X) andalso X>=0 andalso is_string(T);
is_string(_) -> 
    false.

%% wol_yaml:test().
test() ->
    Input= <<"- name: ENG.1
  bf_league_id: 10932509
  oc_path: \"/football/english/premier-league\"
  fd_league_id: E0
  fd_season_id: \"1819\"
  teams:
  - name: Arsenal
  - name: \"Brighton\"
    alt_names:
    - \"Brighton & Hove Albion\"
    - \"Brighton and Hove Albion\"">>,
    Output=[[#{<<"bf_league_id">> => 10932509,
	       <<"fd_league_id">> => <<"E0">>,
	       <<"fd_season_id">> => <<"1819">>,
	       <<"name">> => <<"ENG.1">>,
	       <<"oc_path">> => <<"/football/english/premier-league">>,
	       <<"teams">> => [#{<<"name">> => <<"Arsenal">>},
			       #{<<"alt_names">> => [<<"Brighton & Hove Albion">>,
						     <<"Brighton and Hove Albion">>],
				 <<"name">> => <<"Brighton">>}]}]],
    Output=deyamerlise(yamerl:decode(Input)),
    ok.
