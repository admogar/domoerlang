%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Client of the system. Starts and stops the master node.
%%% @see master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(client).
-include("client.hrl").

%% PUBLIC API
-export([start/0, stop/0]).
-export([add/2, listGroups/0, checkGroup/1, ping/1, getSensorValue/2]).
-export([upgrade/0, version/0]).

%%--------------------------------------------------------------------
%% @doc Starts the master.
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    master:start(),
    ok.

%%--------------------------------------------------------------------
%% @doc Stops the master.
%% @spec stop() -> ok | timeout 
%% @end
%%--------------------------------------------------------------------
stop() ->
    master:stop().

%%--------------------------------------------------------------------
%% @doc Adds a sensor to a group.
%% @spec add(NombreGrupo :: string(), IdSensor :: string()) -> ok
%% @end
%%--------------------------------------------------------------------
add(NombreGrupo, IdSensor) ->
    master:anadir_sensor(NombreGrupo, IdSensor),
    ok.
    
%%--------------------------------------------------------------------
%% @doc Returns a list containing the groups' names.
%% @spec listGroups() -> ListaNombreGrupos :: list(string()) | [error]
%% @end
%%--------------------------------------------------------------------
listGroups() ->
    master:obtener_grupos().
    
%%--------------------------------------------------------------------
%% @doc Gets a list of states of a group of sensors.
%% @spec checkGroup(NombreGrupo :: string()) ->
%%                           list({NombreSensor :: string(),
%%                                 CacheValor,
%%                                 DiferenciaSegundos :: integer()})
%%                            | timeout
%%       CacheValor = integer() | boolean()
%% @end
%%--------------------------------------------------------------------
checkGroup(NombreGrupo) ->
    master:obtener_estado_grupo(NombreGrupo).
    
%%--------------------------------------------------------------------
%% @doc Gets the state of a sensor given its group's name.
%% @spec getSensorValue(NombreGrupo :: string(),
%%                            NombreSensor :: string()) ->
%%                            Estado
%%                            | {error, grupo_inexistente}
%%                            | {error, sensor_inexistente}
%%       Estado = integer() | boolean()
%% @end
%%--------------------------------------------------------------------
getSensorValue(NombreGrupo, NombreSensor) ->
    master:obtener_valor_sensor(NombreGrupo, NombreSensor).

%%--------------------------------------------------------------------
%% @doc Updates the server to the last compiled version.
%% @spec upgrade() -> ok | timeout
%% @end
%%--------------------------------------------------------------------
upgrade() ->
    master:upgrade().

%%--------------------------------------------------------------------
%% @doc Prints the version on execution.
%% @spec version() -> string() | timeout
%% @end
%%--------------------------------------------------------------------
version() ->
    master:version().
    
%%--------------------------------------------------------------------
%% @doc Checks if a group is alive.
%% @spec ping(NombreGrupo :: string()) ->
%%               {Time :: integer(), pong | timeout}
%% @end
%%--------------------------------------------------------------------
ping(NombreGrupo) ->
    timer:tc(master, ping, [NombreGrupo]).
    
%%% Internal Implementation
