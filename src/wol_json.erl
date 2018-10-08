-module(wol_json).

-export([to_json/1,
	 from_json/1,
	 test/0]).

-import(wol_utils, [template/2]).

-define(DATETIME_PATTERN, <<"^\\d{4}\\-\\d{1,2}\\-\\d{1,2}((\\s|T)\\d{1,2}\\:\\d{1,2}\\:\\d{1,2}(\\.\\d+Z)?)?$">>).
-define(DATE_PATTERN, <<"^\\d{4}\\-\\d{1,2}\\-\\d{1,2}$">>).
-define(SCORE_PATTERN, <<"^\\d{1,2}\\-\\d{1,2}$">>).

to_json(Struct) ->
    jsx:encode(recurse(Struct, fun(Value) -> to_string(Value) end)).

from_json(Struct) ->
    recurse(jsx:decode(Struct, [return_maps]), fun(Value) -> from_string(Value) end).
			 
recurse(Struct, Encoder) when is_map(Struct) ->
    recurse(maps:to_list(Struct), Encoder);
recurse(Struct, Encoder) ->
    recurse(Struct, Encoder, []).

recurse([], _Encoder, [{_, _}|_]=Results) ->
    maps:from_list(Results);
recurse([], _Encoder, Results) ->
    lists:reverse(Results);
recurse([Item|Items], Encoder, Results) when is_list(Item) ->
    recurse(Items, Encoder, [recurse(Item, Encoder, [])|Results]);
recurse([Item|Items], Encoder, Results) when is_map(Item) ->
    recurse(Items, Encoder, [recurse(maps:to_list(Item), Encoder, [])|Results]);
recurse([{K, V}|Items], Encoder, Results) when is_list(V)->
    recurse(Items, Encoder, [{K, recurse(V, Encoder, [])}|Results]);
recurse([{K, V}|Items], Encoder, Results) when is_map(V)->
    recurse(Items, Encoder, [{K, recurse(maps:to_list(V), Encoder, [])}|Results]);
recurse([{K, V}|Items], Encoder, Results) ->
    recurse(Items, Encoder, [{K, Encoder(V)}|Results]);
recurse([V|Items], Encoder, Results) ->
    recurse(Items, Encoder, [Encoder(V)|Results]).

to_string({{_Y, _M, _D}, {0, 0, 0}}=Date) ->
    qdate:to_string(<<"Y-m-d">>, Date);
to_string({{_Y, _M, _D}, {_H, _MM, _S}}=Date) ->
    qdate:to_string(<<"Y-m-d H:i:s">>, Date);
to_string({HG, AG}) ->
    template(<<"~p-~p">>, [HG, AG]);
to_string(Value) ->
    Value.

from_string(Value) when is_binary(Value) ->
    from_string(Value, {re:run(Value, ?DATETIME_PATTERN)
		       ,re:run(Value, ?DATE_PATTERN),
			re:run(Value, ?SCORE_PATTERN)});
from_string(Value) ->
    Value.

from_string(Value, {{match, _}, _, _}) ->
    qdate:to_date(Value);
from_string(Value, {_, {match, _}, _}) ->
    qdate:to_date(Value);
from_string(Value, {_, _, {match, _}}) ->
    list_to_tuple([list_to_integer(Tok) || Tok <- re:split(Value, <<"-">>, [{return, list}])]);
from_string(Value, _) ->
    Value.

%% wol_json:test().
test() ->
    %% datetime
    DateTime=[#{<<"date">> => {{1970, 1, 1}, {1, 2, 3}}}],
    DateTimeStr= <<"[{\"date\":\"1970-01-01 01:02:03\"}]">>,
    DateTimeStr2= <<"[{\"date\":\"1970-01-01T01:02:03.000Z\"}]">>,
    DateTime=from_json(DateTimeStr),
    DateTime=from_json(DateTimeStr2),
    DateTimeStr=to_json(DateTime),
    %% date
    Date=[#{<<"date">> => {{1970, 1, 1}, {0, 0, 0}}}],
    DateStr= <<"[{\"date\":\"1970-01-01\"}]">>,
    Date=from_json(DateStr),
    DateStr=to_json(Date),
    %% score
    Score=[#{<<"score">> => {2, 1}}],
    ScoreStr= <<"[{\"score\":\"2-1\"}]">>,
    Score=from_json(ScoreStr),
    ScoreStr=to_json(Score),
    ok.
