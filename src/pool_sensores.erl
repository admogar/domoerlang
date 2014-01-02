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
-module(pool_sensores).

-export([start/0]).
-export([get_sensor/1]).

%%% Funciones p�blicas

start() ->
    spawn(fun() -> init() end),
    ok.

%% Dado un nombre de sensor devuelve una tupla (Tipo, PidSensor)
get_sensor(NombreSensor) ->
    ?MODULE ! {self(), getSensor, {NombreSensor}},
    receive
        SensorOrNull -> SensorOrNull
    end.

%%% Funciones privadas
    
init() ->
    register(?MODULE, self()),
    loop(cargar_sensores()).

loop(Sensores) ->
    receive
        {From, getSensor, {NombreSensor}} ->
            case lists:keyfind(NombreSensor, 1, Sensores) of
                {NombreSensor, Tipo, PidSensor} ->
                    From ! {Tipo, PidSensor}
                ; false -> From ! null
            end,
            loop(Sensores)
        ; _ -> loop(Sensores)
    end.


%%% Devuelve una lista de registros de sensores: {nombre, tipo, pid}
cargar_sensores() ->
    [
        ["luz"        , boolean , sensor:nuevo_sensor(boolean)] ,
        ["temperatura", numerico, sensor:nuevo_sensor(numerico)]
    ]
    .
