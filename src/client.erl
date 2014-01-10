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
-export([start/0, stop/1]).
-export([add/3, listGroups/1, checkGroup/2, ping/2]).
-export([upgrade/1, version/1]).

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
stop(MasterNode) ->
    master:stop(MasterNode),
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a new sensor and group.
%% @spec add(Group :: string(), Sensor :: string(), MasterNode :: node()) ->
%%           void
%% @end
%%--------------------------------------------------------------------
add(NombreGrupo, IdSensor, MasterNode) ->
    master:anadir_sensor(NombreGrupo, IdSensor, MasterNode),
    ok.
    
%%--------------------------------------------------------------------
%% @doc Lists existing groups.
%% @spec listMonitors(MasterNode :: pid()) ->
%%                    list(string()) | timeout
%% @end
%%--------------------------------------------------------------------
listGroups(MasterNode) ->
    master:obtener_grupos(MasterNode).
    
%%--------------------------------------------------------------------
%% @doc Checks state of a group.
%% @spec checkMonitor(GroupName :: string(), MasterNode :: node()) ->
%%                    Value :: list() | timeout
%% @end
%%--------------------------------------------------------------------
checkGroup(NombreGrupo, MasterNode) ->
    master:obtener_estado_grupo(NombreGrupo, MasterNode).
    
%%--------------------------------------------------------------------
%% @doc Updates the server to the last compiled version.
%% @spec upgrade(MasterNode :: node()) -> ok | timeout
%% @end
%%--------------------------------------------------------------------
upgrade(MasterNode) ->
    master:upgrade(MasterNode).

%%--------------------------------------------------------------------
%% @doc Prints the version on execution.
%% @spec version(node()) ->
%%               nonempty_string() |
%%               timeout
%% @end
%%--------------------------------------------------------------------
version(MasterNode) ->
    master:version(MasterNode).
    
%%--------------------------------------------------------------------
%% @doc Pings the specified group.
%% @spec ping(string(), node()) ->
%%               pong |
%%               timeout
%% @end
%%--------------------------------------------------------------------
ping(NombreGrupo, MasterNode) ->
    master:ping(NombreGrupo, MasterNode).
    
%%% Internal Implementation
