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
%%% @see grupo
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(monitor).

-include("client.hrl").

%% PUBLIC API
-export([start_link/2, init/2]).

-export([configurar_padre/2 , start/1, pause/1]).

%%--------------------------------------------------------------------
%% @doc Starts the monitor.
%% @spec start_link(GroupPid :: pid())
%% @end
%%--------------------------------------------------------------------
start_link(GroupPid, SensorId) ->
    spawn_link(fun() -> init(GroupPid, SensorId) end).

%%--------------------------------------------------------------------
%% @doc Inits or continues the monitor's execution.
%% @end
%%--------------------------------------------------------------------
init(GroupPid, SensorId) ->
    {SensorType, PidSensor} = sensor_pool:get_sensor(SensorId),
    loop_stopped(GroupPid, PidSensor, SensorType, undefined).

%%--------------------------------------------------------------------
%% @doc Configures a monitor in order to send notifications to a group
%% @spec configurar_padre(PidMonitor :: pid(),
%%                        PidGrupo :: pid())
%%                        -> ok
%% @end
%%--------------------------------------------------------------------
configurar_padre(PidMonitor, PidGrupo) ->
    PidMonitor ! {self(), {configurar_padre, PidGrupo}}.

%%--------------------------------------------------------------------
%% @doc Changes the monitor state to working
%% @spec start(PidMonitor :: pid()) -> starting
%% @end
%%--------------------------------------------------------------------
start(PidMonitor) ->
    PidMonitor ! {self(), start}.    

pause(PidMonitor) ->
    PidMonitor ! {self(), pause}.

%%% Internal implementation %%%

% ESTADO STOPPED ························
loop_stopped(GroupPid, PidSensor, SensorType, Value) ->
    receive
    	{From, ping} ->
    	    From ! {self(), pong},
    	    loop_stopped(GroupPid, PidSensor, SensorType, Value);
            
    	{From, upgrade} ->
    	    From ! {self(), ok},
    	    
            ?MODULE:init(GroupPid, PidSensor, SensorType, Value);
        
        {From, {configurar_padre, NewGroupPid}} ->
    	    From ! {self(), ok},
    	    loop_stopped(NewGroupPid, PidSensor, SensorType, Value) ;
            
    	{From, start} ->
            PidSensor ! {self(), setObserver}, % Establecer el observador del sensor
    	    From ! {self(), starting},         % Pasar a estado start
    	    loop_started(GroupPid, PidSensor, SensorType, Value) ;
    
        _ -> % ignorar cualquier otra cosa (pausado/stop => ignorar)
            loop_stopped(GroupPid, PidSensor, SensorType, Value)
    end.

% ESTADO STARTED ························
loop_started(GroupPid, PidSensor, SensorType, Value) ->
    receive
    	{From, ping} ->
    	    From ! {self(), pong},
    	    loop_stopped(GroupPid, PidSensor, SensorType, Value);
            
    	{From, upgrade} ->
    	    From ! {self(), upgrading},
    	    ?MODULE:init(GroupPid, PidSensor, SensorType, Value);
            
    	{From, getValue} ->
            From ! {self(), Value},
    	    loop_started(GroupPid, PidSensor, SensorType, Value);

        {valor, NewValue} ->
            case SensorType of
                {num,_,_} ->
                    if NewValue - Value > 70 -> GroupPid ! {self(), {heartbeat, NewValue}} end ;
                    
                bin ->
                    if NewValue /= Value -> GroupPid ! {self(), {heartbeat, NewValue}} end
            end,
            loop_started(GroupPid, PidSensor, SensorType, NewValue) ;

        {_From, pause} ->
            loop_stopped(GroupPid, PidSensor, SensorType, Value)
    
    after
    	?TIMEOUT ->
    	    GroupPid ! {self(), heartbeat},
    	    loop_started(GroupPid, PidSensor, SensorType, Value)
    end.
