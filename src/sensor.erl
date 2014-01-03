%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Sensor emulator. Acts like a real component.
%%% @see monitor
%%% @see master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sensor).
-include("client.hrl").

%% PUBLIC API
-define(VERSION, 1).
-export([start/1).

%%--------------------------------------------------------------------
%% @doc Starts the sensor.
%% @spec start(Type :: {num :: atom(), Min :: integer(), Max :: integer()}
%%             | bin :: atom())
%%       -> pid() | exception
%% @end
%%--------------------------------------------------------------------
start(Type) ->
    spawn(fun() -> init(Type) end).

%%% Internal Implementation

init(State) ->
    case State of
	{num, Min, Max} ->
	    initializing({Min, Min, Max});
	bin ->
	    initializing(false)
    end.

initializing(State) ->
    receive
	{From, setObserver} ->
	    From ! {self(), initialized},
	    loop(State, From)
    end.

loop(State, MonitorPID) ->
    timer:sleep(?TIMEOUT),
    random:seed(erlang:now()),
    case State of
	{_Value, Min, Max} ->
	    NewValue = Min+random:uniform(Max+1-Min)-1,
	    NewState = {NewValue, Min, Max};
	_Value ->
	    NewValue = (random:uniform(2) == 1),
	    NewState = NewValue
    end,
    MonitorPID ! {self(), NewValue},
    loop(NewState, MonitorPID).
