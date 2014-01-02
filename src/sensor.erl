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

%%% Ver http://www.erlang.org/doc/programming_examples/records.html
%%% Ver http://www.erlang.org/doc/reference_manual/records.html
-record(estado,{tipo, observador=null, funcion_valor}).

%% PUBLIC API
-define(VERSION,1).

-export([nuevo_sensor/1]).

%% Crea un nuevo sensor con el tipo dado: (boolean | numerico)
nuevo_sensor(Tipo)->
    spawn(?MODULE, fun init/1, #estado{tipo=Tipo}).

%%% Internal Implementation

init(EstadoSensor) ->
    if
        EstadoSensor#estado.tipo == boolean -> FuncionValor = fun booleanos/0
      ; EstadoSensor#estado.tipo == numerico -> FuncionValor = fun numericos/0
    end,
    loop(EstadoSensor#estado{funcion_valor=FuncionValor}).

loop(EstadoSensor) ->
    receive
        {setObservador, {PidMonitorObservador}}->
            loop(EstadoSensor#estado{observador=PidMonitorObservador})
    after
        ?SENSOR_TIMEOUT ->
            RandNumber = random:uniform(2),
            if
                RandNumber == 1 and is_function(EstadoSensor#estado.funcion_valor,0) ->
                    EstadoSensor#estado.observador ! {valor, (EstadoSensor#estado.funcion_valor)() }
            end,
            loop(EstadoSensor)
    end.

booleanos() ->
    RandNumber = random:uniform(2),
    if
        RandNumber == 1 -> false
      ; RandNumber == 2 -> true
    end.

numericos() ->
    random:uniform(100).