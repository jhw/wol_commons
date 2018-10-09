-module(wol_subscriber).

-behaviour(gen_server).

%% API.

-export([start_link/1]).

%% gen_server.

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {channel}).

%% API.

start_link(Channel) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Channel], []).

%% gen_server.

init([Channel]) ->
    erlang:start_timer(0, self(), init),
    {ok, #state{channel=Channel}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% tinymq wants lists not binaries

handle_info({timeout, _, init}, #state{channel=Channel}=State) ->
    tinymq:subscribe(binary_to_list(maps:get(<<"name">>, Channel)), now, self()),
    {noreply, State};
handle_info({_, Timestamp, Messages}, #state{channel=Channel}=State) when is_integer(Timestamp) and is_list(Messages) ->
    io:format("~s [~p]: ~p~n", [?MODULE, Timestamp, Messages]),
    tinymq:subscribe(binary_to_list(maps:get(<<"name">>, Channel)), Timestamp, self()),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
