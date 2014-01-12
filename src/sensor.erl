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
-export([start/1]).

-export([set_observer/1]).

%%--------------------------------------------------------------------
%% @doc Starts the sensor.
%% @spec start(Type) -> pid() | exception
%% Type = tuple() | bin
%% Tuple = {num, Min :: integer(), Max :: integer()}
%% @end
%%--------------------------------------------------------------------
start(Type) ->
    spawn(fun() -> init(Type) end).

set_observer(SensorPid) ->
    SensorPid ! {self(), setObserver}.

%%% Internal Implementation


init(Type) ->
    random:seed(erlang:now()),
    case Type of
    	{num, Min, Max} ->
    	    loop({Min, Min, Max}, observador_indefinido) ;
        
    	bin ->
    	    loop({binario,false}, observador_indefinido) ;

        _ ->
            io:format("Error, valor de tipo desconocido: ~p", Type),
            throw(tipo_desconocido)
    end.



loop(State, MonitorPID) ->
    receive
        {FromMonitor, setObserver} ->
            loop(State, FromMonitor)
    
    after ?TIMEOUT->
        if
            MonitorPID/=observador_indefinido ->
                case State of    
                
                    {_Value, Min, Max} ->
                	    NewValue = Min+random:uniform(Max+1-Min)-1,
                        MonitorPID ! {valor, NewValue},
                	    NewState = {NewValue, Min, Max},
                        loop(NewState, MonitorPID) ;
                
                    {binario, _Value} ->
                	    NewValue = (random:uniform(2) == 1),
                        MonitorPID ! {valor, NewValue} ,
                        NewState = {binario, NewValue},
                        loop(NewState, MonitorPID) ;
                    
                    _ -> 
                         io:format("Error, valor de estado desconocido: ~p", State),
                         throw(valor_inesperado)
                end
          ; true ->
                %ignorar observador indefinido
                loop(State, MonitorPID)
        end
    end.
