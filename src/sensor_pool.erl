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

%% PUBLIC API
-export([start/0]).
-export([get_sensor/1]).

%%--------------------------------------------------------------------
%% @doc Starts the sensor pool creation.
%% @spec start() -> ok | exception
%% @end
%%--------------------------------------------------------------------
start() ->
    spawn(fun() -> init() end),
    ok.

%%--------------------------------------------------------------------
%% @doc Gets a sensor. Gets the sensor labeled with the given atom.
%% @spec getSensor(SensorName) -> {Type :: atom(),
%%                                 SensorPID :: pid()}
%%                                | null
%% @end
%%--------------------------------------------------------------------
get_sensor(SensorName) ->
    ?MODULE ! {self(), getSensor, SensorName},
    receive
        PidSensor -> PidSensor
    end.

%%% Internal implementation %%%
    
init() ->
    register(?MODULE, self()),
    loop(sensor_load()).

loop(SensorList) ->
    receive
        {From, getSensor, SensorName} ->
            case lists:keyfind(SensorName, 1, SensorList) of
                {SensorName, SensorType, PidSensor} ->
                    From ! {SensorType, PidSensor};
        		
                false ->
                    From ! null
            end,
            loop(SensorList);
    	
        _ ->
    	    loop(SensorList)
    end.

sensor_load() ->
    [
        ["luz", bin, sensor:start(bin)],
        ["temperatura", num, sensor:start({num,0,100})]
    ].
