-module(client).
-include("client.hrl").

-export([start/0, stop/1, upgrade/1, version/1]).

%%% start
%%%
%%% Inicia el maestro
%%%
start() ->
    ?MASTER:start().

%%% stop
%%%
%%% Para el maestro
%%%
stop(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), {stop}},
    receive
      {?MASTER, State} ->
        State                                                                                               
    after 3000 ->
        timeout
    end.

%%% upgrade
%%% 
%%% Actualiza servidor.
%%% 
upgrade(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), {upgrade}},
    receive
    {?MASTER, ok} ->
	    ok
    after 3000 ->
        timeout
    end.


%%% version
%%% 
%%% Que version estamos usando.
%%% 
version(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), {version}},
    receive
      {?MASTER, Version} ->
        Version
    after 3000 ->
	timeout
    end.
