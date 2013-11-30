-module(monitor).

-include("client.hrl").

-export([start_link/0,loop/0]).

start_link() ->
    spawn_link(fun() -> init() end).

init() ->
    loop().

loop() ->
    receive
	  Msg ->
	    io:format("[~p] WTF? ~p~n", [?MODULE, Msg]),
	    loop()
    end.
