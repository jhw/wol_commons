-module(wol_bf_api).

-export([rpc_post/1,
	 rpc_post/2,
	 rpc_post/3]).

-import(wol_json, [decode/1,
		   encode/1]).

-define(API_URL, <<"https://api.betfair.com/exchange/betting/json-rpc/v1">>).
-define(USER_AGENT, <<"Erlang/OTP 20">>).
-define(APPLICATION_JSON, <<"application/json">>).
-define(BF_DEFAULT_TIMEOUT, 5000). 

rpc_post(ReqBody) ->
    rpc_post(bfa_token_server:token(), ReqBody).

rpc_post(undefined, _ReqBody) ->
    {error, <<"BF token not found">>};
rpc_post(Token, ReqBody) ->
    rpc_post(Token, ReqBody, ?BF_DEFAULT_TIMEOUT).

rpc_post(Token, ReqBody, Timeout) ->
    case httpc:request(post, 
		       {binary_to_list(?API_URL),
			rpc_headers(maps:get(<<"app_key">>, get(bf_creds)), Token),
			binary_to_list(?APPLICATION_JSON),
			binary_to_list(encode(ReqBody))},
		       rpc_options(maps:get(<<"cert_file">>, get(bf_creds)),
				   maps:get(<<"key_file">>, get(bf_creds)),
				   Timeout),
		       []) of 	
	{ok, {{_Version, 200, _Msg}, _Headers, RespBody}} ->
	    RespStruct=decode(list_to_binary(RespBody)),
	    case maps:find(<<"result">>, RespStruct) of
		{ok, Result} ->
		    {ok, Result};
		error ->
		    {error, RespStruct}
	    end;
	{ok, {{_Version, _, _Msg}, _Headers, RespBody}} ->
	    {error, RespBody};
	{error, timeout} ->
	    {error, <<"timeout">>};
	Other ->
	    {error, Other}
    end.

rpc_headers(AppKey) ->
    [{"User-Agent", binary_to_list(?USER_AGENT)}, 
     {"X-Application", binary_to_list(AppKey)}, 
     {"Accept", binary_to_list(?APPLICATION_JSON)}]. 

rpc_headers(AppKey, Token) ->
    rpc_headers(AppKey)++[{"X-Authentication", binary_to_list(Token)}].

rpc_options(CertFile, KeyFile, Timeout) ->
    [{ssl, [{verify, 0},
	    {certfile, binary_to_list(CertFile)},
	    {keyfile, binary_to_list(KeyFile)}]},
     {timeout, Timeout},
     {autoredirect, false}].

