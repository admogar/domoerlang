-module(master).
-include("client.hrl").

-define(VERSION,1).

-export([start/0,loop/0]).

start() ->
    spawn(fun() -> init() end),
    ok.

init() ->
    register(?MASTER, self()), 
    loop().

loop() ->
  receive
    {From, {version}} ->
      From ! {?MASTER, ?VERSION},
      loop();
    {From, {upgrade}} ->
      From ! {?MASTER,ok},
      ?MODULE:loop();
    {From, {stop}} ->
      From ! {?MASTER, stopping} 
  end.

