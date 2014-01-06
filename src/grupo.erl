%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc
%%%
%%% =Nodo grupo=
%%%
%%% El master posee una serie de nodos grupo. Cada nodo grupo controla varios monitores.
%%% 
%%% El master ha de crear un nodo grupo e iterativamente asignar los monitores
%%% a dicho grupo.     
%%% 
%%% == Funcionalidades ==
%%% 
%%% === Crear un grupo ===
%%% El master *ha de crear el grupo*, siendo el master quien decide si desea enlazarse o no
%%% (el enlazamiento dependerá de quién esté observando, no al revés).
%%% 
%%% Para ello se dispone de la función {@link crear_grupo/0}
%%% Por cuestiones de comodidad se dispone también de la función {@link crear_y_enlazar/0} 
%%%
%%% === Añadir monitor al grupo ===
%%% Para añadir un monitor se dispone de la función {@link añadir_monitor/1}
%%%
%%%
%%%
%%% @see client
%%% @see monitor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(grupo).
-include("client.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([crear/0, crear_y_enlazar/0]).

-export([anadir_sensor/2, obtener_estado/1]).

-record(infoMonitor, {pid_monitor, nombre_sensor, cache_valor, heartbeat_timestamp}).

%%% crear(): pid()
crear() ->
    spawn(?MODULE, fun() -> init() end, []).

%%% crear_y_enlazar(): pid()
crear_y_enlazar() ->
    spawn_link(?MODULE, fun() -> init() end, []).
    
%%% anadir_sensor(PidGrupo : pid(), IdSensor: string())
anadir_sensor(PidGrupo, IdSensor) ->
    PidGrupo ! {self(), {anadir, IdSensor}}.

% Devuelve una lista con lo siguiente:
% [nombre sensor, valor en cache, tiempo desde el ultimo heartbeat]
% nonmbre sensor: string
% valor en cache: depende del sensor
% tiempo desde el último heartbeat: segundos
obtener_estado(PidGrupo) ->
    PidGrupo ! {self(), {obtenerEstado}},
    receive
        {info_grupo, ListaEstados} -> ListaEstados
    after
        ?TIMEOUT -> [error]
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%%% Ver http://www.erlang.org/doc/reference_manual/records.html
%%% Ver http://www.erlang.org/doc/programming_examples/records.html

init() ->
    process_flag(trap_exit, true),
    loop(lists:new()). % Lista vacía de monitores

% Monitores :: list(#infoMonitor, #infoMonitor, ...)
loop(Monitores) ->
    receive

        {_From, {anadir, IdSensor}} ->
            % Si ya contenemos el sensor, ignoramos
            case lists:keyfind(IdSensor, #infoMonitor.nombre_sensor, Monitores) of
                InfoMonitor when is_record(InfoMonitor, infoMonitor) ->
                    loop(Monitores)
                ; false ->
                    % Si no contenemos el monitor lo añadimos y configuramos
                    NuevoMonitor = #infoMonitor{pid_monitor=monitor:start_link(self(), IdSensor),
                                                nombre_sensor=IdSensor,
                                                heartbeat_timestamp=os:timestamp()},
                    NuevosMonitores = [NuevoMonitor| Monitores],
                    monitor:configurar_padre(NuevoMonitor#infoMonitor.pid_monitor, self()), % Monitor notificará a este grupo (self)

                    loop(NuevosMonitores)
            end

        ; {PidMonitor, heartbeat} ->  % Sustituimos el último timestamp de heartbeat por el nuevo
            EstadoMonitorAntiguo = lists:keyfind(PidMonitor, #infoMonitor.pid_monitor, Monitores),
            NuevoEstadoMonitor = EstadoMonitorAntiguo#infoMonitor{heartbeat_timestamp=os:timestamp()},
            loop(lists:keyreplace(PidMonitor, #infoMonitor.pid_monitor, Monitores, NuevoEstadoMonitor))
       
        ; {PidMonitor, {heartbeat, Valor}} -> % Heartbeat con nuevo valor
            EstadoMonitorAntiguo = lists:keyfind(PidMonitor, #infoMonitor.pid_monitor, Monitores),
            NuevoEstadoMonitor = EstadoMonitorAntiguo#infoMonitor{heartbeat_timestamp=os:timestamp(),
                                                                  cache_valor=Valor},
            loop(lists:keyreplace(PidMonitor, #infoMonitor.pid_monitor, Monitores, NuevoEstadoMonitor))         
    
        ; {From, obtenerEstado} ->
            TimestampAhora=os:timestamp(),
            From ! {info_grupo, [[EstadoMonitor#infoMonitor.nombre_sensor,
                                  EstadoMonitor#infoMonitor.cache_valor,
                                  diferencia_segundos(EstadoMonitor#infoMonitor.heartbeat_timestamp, TimestampAhora)] || EstadoMonitor <- Monitores]
                   }
    
        ; _ -> false
    end.

diferencia_segundos(EstadoMonitor, TimestampAhora) ->
    {_, SegundosMonitor, _} = EstadoMonitor,
    {_, SegundosAhora, _} = TimestampAhora,
    SegundosAhora - SegundosMonitor.

