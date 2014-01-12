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
-export([crear/1, crear_y_enlazar/1]).

-export([anadir_sensor/2, obtener_estado/1, obtener_valor_sensor/2, ping/1]).

-record(infoMonitor, {pid_monitor, nombre_sensor, cache_valor, heartbeat_timestamp}).

%%% crear(PidMaster): pid()
%%--------------------------------------------------------------------
%% @doc Creates a group.
%% @end
%%--------------------------------------------------------------------
crear(PidMaster) ->
    spawn(fun() -> init(PidMaster) end).

%%% crear_y_enlazar(PidMaster): pid()
%%--------------------------------------------------------------------
%% @doc Creates a group and links the caller process to it.
%% @end
%%--------------------------------------------------------------------
crear_y_enlazar(PidMaster) ->
    spawn_link(fun() -> init(PidMaster) end).
    
%%% anadir_sensor(PidGrupo : pid(), IdSensor: string())
%%--------------------------------------------------------------------
%% @doc Adds a sensor to a group. If the group doesn't exist, also
%% creates it.
%% @end
%%--------------------------------------------------------------------
anadir_sensor(PidGrupo, IdSensor) ->
    PidGrupo ! {self(), {anadir, IdSensor}}.

% Devuelve una lista con lo siguiente:
% [nombre sensor, valor en cache, tiempo desde el ultimo heartbeat]
% nonmbre sensor: string
% valor en cache: depende del sensor
% tiempo desde el último heartbeat: segundos
%%--------------------------------------------------------------------
%% @doc Gets a list of states of a group of sensors.
%% @spec obtener_estado(PidGrupo :: pid()) ->
%%                      list({NombreSensor :: string(),
%%                            CacheValor,
%%                            DiferenciaSegundos :: integer()})
%%                      | timeout
%%       CacheValor = integer() | boolean()
%% @end
%%--------------------------------------------------------------------
obtener_estado(PidGrupo) ->
    PidGrupo ! {self(), obtenerEstado},
    receive
        {info_grupo, ListaEstados} -> ListaEstados
    after
        ?TIMEOUT -> {error}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the actual value of a sensor.
%% @spec obtener_valor_sensor(PidGrupo :: pid(),
%%                            NombreSensor :: string()) ->
%%                            ValorOrError :: {valor_sensor,
%%                                             CacheValor}
%%                            | {error, sensor_inexistente}
%%                            | {error}
%%        CacheValor = integer() | boolean()
%% @end
%%--------------------------------------------------------------------
obtener_valor_sensor(PidGrupo, NombreSensor) ->
    PidGrupo ! {self(), obtener_valor_sensor, NombreSensor},
    receive ValorOrError -> ValorOrError
    after ?TIMEOUT -> {error}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a group is alive
%% @spec ping(PidGrupo :: pid()) -> pong | {error}
%% @end
%%--------------------------------------------------------------------
ping(PidGrupo) ->
    PidGrupo ! {self(), ping},
    receive
        {PidGrupo, pong} -> pong
    after
        ?TIMEOUT -> {error}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%%% Ver http://www.erlang.org/doc/reference_manual/records.html
%%% Ver http://www.erlang.org/doc/programming_examples/records.html

init(PidMaster) ->
    process_flag(trap_exit, true),
    loop([], PidMaster). % Lista vacía de monitores y el pid del master

% Monitores :: list(#infoMonitor, #infoMonitor, ...)
loop(Monitores, PidMaster) ->
    receive

        {_From, {anadir, IdSensor}} ->
            % Si ya contenemos el sensor, ignoramos
            case lists:keyfind(IdSensor, #infoMonitor.nombre_sensor, Monitores) of
                InfoMonitor when is_record(InfoMonitor, infoMonitor) ->
                    loop(Monitores, PidMaster)
                ; false ->
                    % Si no contenemos el monitor lo añadimos y configuramos
                    NuevoMonitor = #infoMonitor{pid_monitor=monitor:start_link(self(), IdSensor),
                                                nombre_sensor=IdSensor,
                                                heartbeat_timestamp=os:timestamp()},
                    NuevosMonitores = [NuevoMonitor| Monitores],
                    monitor:configurar_padre(NuevoMonitor#infoMonitor.pid_monitor, self()), % Monitor notificará a este grupo (self)

                    loop(NuevosMonitores, PidMaster)
            end

        ; {PidMonitor, heartbeat} ->  % Sustituimos el último timestamp de heartbeat por el nuevo
            EstadoMonitorAntiguo = lists:keyfind(PidMonitor, #infoMonitor.pid_monitor, Monitores),
            NuevoEstadoMonitor = EstadoMonitorAntiguo#infoMonitor{heartbeat_timestamp=os:timestamp()},
            loop(lists:keyreplace(PidMonitor, #infoMonitor.pid_monitor, Monitores, NuevoEstadoMonitor), PidMaster)
       
        ; {PidMonitor, heartbeat, Valor} -> % Heartbeat con nuevo valor
            EstadoMonitorAntiguo = lists:keyfind(PidMonitor, #infoMonitor.pid_monitor, Monitores),
            NuevoEstadoMonitor = EstadoMonitorAntiguo#infoMonitor{heartbeat_timestamp=os:timestamp(),
                                                                  cache_valor=Valor},
            loop(lists:keyreplace(PidMonitor, #infoMonitor.pid_monitor, Monitores, NuevoEstadoMonitor), PidMaster)
    
        ; {From, ping} ->
            From ! {self(),pong},
            loop(Monitores, PidMaster)
        ; {From, obtenerEstado} ->
            TimestampAhora=os:timestamp(),
            From ! {info_grupo, [{EstadoMonitor#infoMonitor.nombre_sensor,
                                  EstadoMonitor#infoMonitor.cache_valor,
                                  diferencia_segundos(EstadoMonitor#infoMonitor.heartbeat_timestamp, TimestampAhora)} || EstadoMonitor <- Monitores]
                   },
            loop(Monitores, PidMaster)

        ; {From, obtener_valor_sensor, NombreSensor} ->
            case lists:keyfind(NombreSensor, #infoMonitor.nombre_sensor, Monitores) of
                false ->
                    From ! {error, sensor_inexistente}
              ; InfoMonitor ->
                    From ! {valor_sensor, InfoMonitor#infoMonitor.cache_valor}
            end,
            loop(Monitores, PidMaster)
    
        ; {'EXIT', PidMaster, _Reason} -> fin_de_master % Finalizamos la ejecución de grupo porque el master ha finalizado
    
        ; MensajeInesperado ->
            io:format("Mensaje inesperado: ~p~n", [MensajeInesperado]),
            loop(Monitores, PidMaster)
    end.

diferencia_segundos(EstadoMonitor, TimestampAhora) ->
    {_, SegundosMonitor, _} = EstadoMonitor,
    {_, SegundosAhora, _} = TimestampAhora,
    SegundosAhora - SegundosMonitor.

