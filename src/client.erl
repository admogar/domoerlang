-module(client).
-include("client.hrl").

-export([start/0, stop/1, add/2, listMonitors/1, checkMonitor/2, upgrade/1, version/1]).

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
    after ?TIMEOUT ->
        timeout
    end.

%%% add
%%%
%%% Añadir nuevo monitor
%%%
add(MasterNode, Monitor) ->
    {?MASTER, MasterNode} ! {self(), {add,Monitor}},
    receive
      {?MASTER, State} ->
        State
    after ?TIMEOUT ->
        timeout
    end.
    
%%% listMonitors
%%% 
%%% Listar monitores activos
%%% 
listMonitors(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), {list_monitors}},
    receive
      {?MASTER, List} ->
        List
    after ?TIMEOUT ->
        timeout
    end.
    
%%% checkMonitor
%%% 
%%% Comprobar estado sensor
%%% 
checkMonitor(MasterNode, Monitor) ->
    {?MASTER, MasterNode} ! {self(), {check, Monitor}},
    receive
      {?MASTER, Value} ->
        Value
    after ?TIMEOUT ->
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
    after ?TIMEOUT ->
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
    after ?TIMEOUT ->
	timeout
    end.
