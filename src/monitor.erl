-module(monitor).

-include("client.hrl").

-export([start_link/0,loop/0]).

start_link() ->
    spawn_link(fun() -> init() end).

init() ->
    loop().

%% {From, {ping}} - Se recibe peticion de master para comprobar estado sensor
loop() ->
    receive
      {From, {ping}} ->
        From ! {self(),{pong,37}}, %%TODO Ahora devuelve numero fijo, crear sensores
        loop();
	  Msg ->
	    io:format("[~p] WTF? ~p~n", [?MODULE, Msg]),
	    loop()
    end.
