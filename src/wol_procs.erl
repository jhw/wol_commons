-module(wol_procs).

-export([init_permanent_worker/2,
	 init_permanent_worker/3,
	 init_transient_worker/2,
	 init_transient_worker/3,
	 init_temporary_worker/2,
	 init_temporary_worker/3,
	 init_permanent_sup/2,
	 init_permanent_sup/3]).

-define(PERMANENT, permanent).
-define(TRANSIENT, transient).
-define(TEMPORARY, temporary).

init_permanent_worker(Mod, Args) ->
    init_worker(Mod, Mod, Args, ?PERMANENT).

init_permanent_worker(Server, Mod, Args) ->
    init_worker(Server, Mod, Args, ?PERMANENT).

init_transient_worker(Mod, Args) ->
    init_worker(random_id(32), Mod, Args, ?TRANSIENT).

init_transient_worker(Server, Mod, Args) ->
    init_worker(Server, Mod, Args, ?TRANSIENT).

init_temporary_worker(Mod, Args) ->
    init_worker(random_id(32), Mod, Args, ?TEMPORARY).

init_temporary_worker(Server, Mod, Args) ->
    init_worker(Server, Mod, Args, ?TEMPORARY).

init_permanent_sup(Mod, Args) ->
    init_sup(Mod, Mod, Args, ?PERMANENT).

init_permanent_sup(Server, Mod, Args) ->
    init_sup(Server, Mod, Args, ?PERMANENT).

init_worker(Server, Mod, Args, Type) ->
    {Server,
     {Mod, start_link, Args},
     Type,
     10500,
     worker, 
     [Mod]}.

init_sup(Server, Mod, Args, Type) ->
    {Server,
     {Mod, start_link, Args},
     Type,
     10500,
     supervisor, 
     [Mod]}.

random_id(N) ->
    list_to_binary([65+round((26*rand:uniform())-0.5) || _ <- lists:seq(1, N)]).
