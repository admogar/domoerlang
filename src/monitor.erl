%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Monitor of an automation component. Checks its state and,
%%% if that's the case, sends orders to it.
%%% @see master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(monitor).

-include("client.hrl").

%% PUBLIC API
-export([start_link/0, init/0]).

%%--------------------------------------------------------------------
%% @doc Starts the monitor.
%% @spec start_link() -> ok | exception
%% @end
%%--------------------------------------------------------------------
start_link() ->
    spawn_link(fun() -> init() end),
    ok.

%%--------------------------------------------------------------------
%% @doc Inits or continues the monitor's execution.
%% @end
%%--------------------------------------------------------------------
init() ->
    loop().

%%% Internal implementation %%%

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
