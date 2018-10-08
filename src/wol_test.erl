-module(wol_test).

-export([test/0]).

%% wol_test:test().
test() ->
    io:format("testing~n"),
    ok=wol_json:test(),
    ok=wol_yaml:test(),
    ok.
