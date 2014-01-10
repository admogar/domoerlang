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
%% @doc Commands the master to start.
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    master:start(),
    ok.

%%--------------------------------------------------------------------
%% @doc Commands the master to stop.
%% @spec stop(MasterNode :: pid()) ->
%%            State :: atom() |
%%                     timeout 
%% @end
%%--------------------------------------------------------------------
stop() ->
    master:stop(),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a new sensor and group.
%% @spec add(Group :: string(), Sensor :: string(), MasterNode :: node()) ->
%%           void
%% @end
%%--------------------------------------------------------------------
add(NombreGrupo, IdSensor) ->
    master:anadir_sensor(NombreGrupo, IdSensor),
    ok.
    
%%--------------------------------------------------------------------
%% @doc Lists existing groups.
%% @spec listMonitors(MasterNode :: pid()) ->
%%                    list(string()) | timeout
%% @end
%%--------------------------------------------------------------------
listGroups() ->
    master:obtener_grupos().
    
%%--------------------------------------------------------------------
%% @doc Checks state of a group.
%% @spec checkMonitor(GroupName :: string(), MasterNode :: node()) ->
%%                    Value :: list() | timeout
%% @end
%%--------------------------------------------------------------------
checkGroup(NombreGrupo) ->
    master:obtener_estado_grupo(NombreGrupo).
    

getSensorValue(NombreGrupo, NombreSensor) ->
    master:obtener_valor_sensor(NombreGrupo, NombreSensor).


%%--------------------------------------------------------------------
%% @doc Updates the server to the last compiled version.
%% @spec upgrade(MasterNode :: node()) -> ok | timeout
%% @end
%%--------------------------------------------------------------------
upgrade() ->
    master:upgrade().

%%--------------------------------------------------------------------
%% @doc Prints the version on execution.
%% @spec version(node()) ->
%%               nonempty_string() |
%%               timeout
%% @end
%%--------------------------------------------------------------------
version() ->
    master:version().
    
%%--------------------------------------------------------------------
%% @doc Pings the specified group.
%% @spec ping(string(), node()) ->
%%               pong |
%%               timeout
%% @end
%%--------------------------------------------------------------------
ping(NombreGrupo) ->
    timer:tc(master, ping, [NombreGrupo]).
    
%%% Internal Implementation
