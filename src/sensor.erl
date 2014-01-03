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
-export([start/2]).

%%--------------------------------------------------------------------
%% @doc Starts the sensor.
%% @spec start(SensorID :: atom(),
%%             Type :: {num :: atom(), Min :: integer(), Max :: integer()}
%%                     | bin :: atom())
%%       -> pid() | exception
%% @end
%%--------------------------------------------------------------------
start(SensorID, Type) ->
    spawn(fun() -> init(SensorID, Type) end).

%%% Internal Implementation

init(SensorID, State) ->
    case State of
	{num, Min, Max} ->
	    initializing(SensorID, {Min, Min, Max});
	bin ->
	    initializing(SensorID, false)
    end.

initializing(SensorID, State) ->
    receive
	{From, setObserver} ->
	    From ! {self(), initialized},
	    loop(SensorID, State, From)
    end.

loop(SensorID, State, MonitorPID) ->
    timer:sleep(?TIMEOUT),
    case State of
	{_Value, Min, Max} ->
	    random:seed(erlang:now()),
	    NewValue = Min+random:uniform(Max+1-Min)-1,
	    MonitorPID ! {self(), NewValue},
	    loop(SensorID, {NewValue, Min, Max}, MonitorPID);
	_Value ->
	    random:seed(erlang:now()),
	    NewValue = (random:uniform(2) == 1),
	    MonitorPID ! {self(), NewValue},
	    loop(SensorID, NewValue, MonitorPID)
    end.
