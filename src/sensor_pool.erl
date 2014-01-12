%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Sensor pool loader. Creates a pool of sensors and turns them
%%% available for monitors to observe.
%%% @see sensor
%%% @see monitor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sensor_pool).
-include("client.hrl").

%% PUBLIC API
-export([start/0]).
-export([get_sensor/1]).

%%--------------------------------------------------------------------
%% @doc Starts the sensor pool creation.
%% @end
%%--------------------------------------------------------------------
start() ->
    case global:whereis_name(?SENSOR_POOL) of
        undefined ->
            spawn(fun() -> init() end)
      ; _PidMaster ->
            throw({error, sensor_pool_already_running})
    end.

%%--------------------------------------------------------------------
%% @doc Gets a sensor PID.
%% @spec get_sensor(SensorName :: string()) ->
%%                 {Type, SensorPID :: pid()} | null
%%       Type = num | bin
%% @end
%%--------------------------------------------------------------------
get_sensor(SensorName) ->
    get_sensor_pool_pid() ! {self(), getSensor, SensorName},
    receive
        {return_getSensor, SensorType, PidSensor} -> {SensorType, PidSensor}
      ; null -> throw ({error, sensor_no_existente})
    end.

%%% Internal implementation %%%

get_sensor_pool_pid() ->
    case global:whereis_name(?SENSOR_POOL) of
        undefined -> start()
      ; PidSensorPool -> PidSensorPool
    end.
    
init() ->
    global:register_name(?SENSOR_POOL, self()),
    loop(sensor_load()).

loop(SensorList) ->
    receive
        {From, getSensor, SensorName} ->
            case lists:keyfind(SensorName, 1, SensorList) of
                {SensorName, SensorType, PidSensor} ->
                    From ! {return_getSensor, SensorType, PidSensor};
        		
                false ->
                    From ! null
            end,
            loop(SensorList);
    	
        _ ->
    	    loop(SensorList)
    end.

sensor_load() ->
    [
        {"luz", bin, sensor:start(bin)},
        {"temperatura", num, sensor:start({num,0,100})}
    ].
