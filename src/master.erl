%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Master node. Receives petitions from the client, sends the
%%% results and handles the monitors.
%%% @see client
%%% @see monitor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(master).
-include("client.hrl").

%% PUBLIC API
-define(VERSION,1).
-export([start/0, stop/0]).

-export([anadir_sensor/2, obtener_grupos/0, obtener_estado_grupo/1, obtener_valor_sensor/2]).
-export([upgrade/0, version/0, ping/1]).

%%% Ver http://erlang.org/doc/man/global.html
%%% Ver http://www.erlang.org/doc/reference_manual/records.html
%%% Ver http://www.erlang.org/doc/programming_examples/records.html
-record(infoGrupo, {nombre, sensores = [], pid_grupo}).

%%--------------------------------------------------------------------
%% @doc Starts the master.
%% @end
%%--------------------------------------------------------------------
start() ->
    case global:whereis_name(?MASTER) of
        undefined ->
            spawn(fun() -> init() end)
      ; _PidMaster ->
            throw({error, master_already_running})
    end.

%%--------------------------------------------------------------------
%% @doc Stops the master.
%% @spec stop() -> ok | timeout
%% @end
%%--------------------------------------------------------------------
stop() ->
    get_master_pid() ! {self(), stop},
    receive
	{?MASTER, stopping} ->
	    ok
    after
	?TIMEOUT ->
	    timeout
    end.

%%--------------------------------------------------------------------
%% @doc Adds a sensor to a group.
%% @end
%%--------------------------------------------------------------------
anadir_sensor(NombreGrupo, IdSensor) ->
    get_master_pid() ! {self(), {add, IdSensor, NombreGrupo}}.

%%--------------------------------------------------------------------
%% @doc Returns a list containing the groups' names.
%% @spec obtener_grupos() -> ListaNombreGrupos :: list(string())
%%                           | [error]
%% @end
%%--------------------------------------------------------------------
obtener_grupos() ->
    get_master_pid() ! {self(), get_lista_grupos},
    receive
        {grupos, ListaNombreGrupos} -> ListaNombreGrupos
    after
        ?TIMEOUT ->
            [error]
    end.

%%--------------------------------------------------------------------
%% @doc Gets a list of states of a group of sensors.
%% @spec obtener_estado_grupo(NombreGrupo :: string()) ->
%%                           list({NombreSensor :: string(),
%%                                 CacheValor :: integer() | boolean(),
%%                                 DiferenciaSegundos :: timestamp()})
%%                            | timeout
%% @end
%%--------------------------------------------------------------------
obtener_estado_grupo(NombreGrupo) ->
    get_master_pid() ! {self(), get_estado_grupo, NombreGrupo},
    receive
        {estado_grupo, ListaEstados} -> ListaEstados
    after
        ?TIMEOUT ->
            timeout
    end.

%%--------------------------------------------------------------------
%% @doc Gets the state of a sensor given its group's name.
%% @spec obtener_valor_sensor(NombreGrupo :: string(),
%%                            NombreSensor :: string()) ->
%%                            Valor :: integer() :: boolean()
%%                            | {error, grupo_inexistente}
%%                            | {error, sensor_inexistente}
%% @end
%%--------------------------------------------------------------------
obtener_valor_sensor(NombreGrupo, NombreSensor) ->
    get_master_pid() ! {self(), get_valor_sensor, NombreGrupo, NombreSensor},
    receive
        {valor_sensor, Valor} -> Valor
      ; {error, grupo_inexistente} -> {error, grupo_inexistente}
      ; {error, sensor_inexistente} -> {error, sensor_inexistente}
    after
        ?TIMEOUT ->
            timeout
    end.

%%--------------------------------------------------------------------
%% @doc Upgrades the master code in execution to the last compiled
%% version.
%% @spec upgrade() -> ok | timeout
%% @end
%%--------------------------------------------------------------------
upgrade() ->
    get_master_pid() ! {self(), upgrade},
    receive
    {?MASTER, upgrading} ->
      ok
    after
    ?TIMEOUT ->
      timeout
    end.

%%--------------------------------------------------------------------
%% @doc Shows the master code version in execution.
%% @spec version() -> Version :: string() | timeout
%% @end
%%--------------------------------------------------------------------
version() ->
    get_master_pid() ! {self(), version},
    receive
    {?MASTER, Version} ->
      Version
    after
    ?TIMEOUT ->
      timeout
    end.
    
%%--------------------------------------------------------------------
%% @doc Checks if a group is alive.
%% @spec ping(NombreGrupo :: string()) -> pong | timeout
%% @end
%%--------------------------------------------------------------------
ping(NombreGrupo) -> 
    get_master_pid() ! {self(), ping, NombreGrupo},
    receive
        {?MASTER, pong} -> pong
    after
        ?TIMEOUT ->
            timeout
    end.

%%% Internal Implementation

init() ->
    global:register_name(?MASTER, self()),
    process_flag(trap_exit, true),
    loop([]).

get_master_pid() ->
    case global:whereis_name(?MASTER) of
        undefined ->
            throw({error, master_not_running})
      ; PidMaster -> PidMaster
    end.

%% {From, {add, Monitor}} - Anadir un monitor
%% {From, list_monitors} - Lista de monitores en master
%% {From, {check, Monitor}} - Comprobar estado monitor
%% {From, version} - Version de master
%% {From, upgrade} - Actualizar master en caliente
%% {From, stop} - Parar master
%% {'EXIT', Pid, Reason} - En caso de error en monitor, reinicio
loop(Grupos) ->
    receive
    	{_From, {add, IdSensor, NombreGrupo}} ->
            % getGrupo() crea y añade el grupo en caso de no existir
            {InfoGrupo, NuevoGrupos} = getGrupo(NombreGrupo, Grupos),
            grupo:anadir_sensor(InfoGrupo#infoGrupo.pid_grupo, IdSensor),
    	    loop(NuevoGrupos) ;

        {From, get_lista_grupos} ->
            From ! {grupos, [ InfoGrupo#infoGrupo.nombre || InfoGrupo <- Grupos]},
            loop(Grupos) ;
        
        {From, get_estado_grupo, NombreGrupo} ->
            {#infoGrupo{pid_grupo=PidGrupo}, Grupos} = getGrupo(NombreGrupo, Grupos),
            From ! {estado_grupo, grupo:obtener_estado(PidGrupo)},
            loop(Grupos) ;

        {From, get_valor_sensor, NombreGrupo, NombreSensor} ->
            case existe_grupo(NombreGrupo, Grupos) of
                false ->
                    From ! {error, grupo_inexistente},
                    loop(Grupos)
              ; true ->
                    {#infoGrupo{pid_grupo=PidGrupo}, Grupos} = getGrupo(NombreGrupo, Grupos),
                    grupo:obtener_valor_sensor(PidGrupo, NombreSensor),
                    receive
                        ValorOrError -> From ! ValorOrError
                    after ?TIMEOUT ->
                        From ! {error}
                    end,
                    loop(Grupos)
            end ;
        
        {From, ping, NombreGrupo} ->
            {#infoGrupo{pid_grupo=PidGrupo}, Grupos} = getGrupo(NombreGrupo, Grupos),
            From ! {?MASTER, grupo:ping(PidGrupo)},
            loop(Grupos) ;
        
        %{From, pong, NombreGrupo} ->
            
        
        {From, version} ->
      	    From ! {?MASTER, ?VERSION},
      	    loop(Grupos);
    	
        {From, upgrade} ->
    	    From ! {?MASTER, upgrading},
                %TODO: PROPAGAR UPGRADE A LISTA
    	    ?MODULE:loop(Grupos);
    	
        {From, stop} ->
            global:unregister_name(?MASTER),
    	    From ! {?MASTER, stopping};
    	
        {'EXIT', Pid, Reason} -> 
    	    io:format("Got exit signal from ~p: ~p~n", [Pid,Reason]),
    	    InfoGrupo = lists:keyfind(Pid, #infoGrupo.pid_grupo, Grupos),
    	    GruposActualizados= lists:keydelete(Pid, #infoGrupo.pid_grupo, Grupos),
    	    io:format("Error in group ~p with process ~p y sensores ~p ~n", [InfoGrupo#infoGrupo.nombre, InfoGrupo#infoGrupo.pid_grupo, InfoGrupo#infoGrupo.sensores]),
    	    simple_smtp_sender:send(?ADMIN_MAIL, ?DOMOERL_MAIL, "Fallo en domoerlang",
                io_lib:format("<!DOCTYPE html><html><body>El proceso ~p encargado de <strong>~p</strong> se ha petado debido a <strong>~p</strong> y ha habido que reiniciarlo.~n
                               <p><img src=\"http://galeri3.uludagsozluk.com/138/facepalm_227785.jpg\" alt=\"Facepalm\"></p></body></html>",
                               [InfoGrupo#infoGrupo.pid_grupo, InfoGrupo#infoGrupo.nombre, Reason]),  ?SMTP_SERV, ?SMTP_PORT),
    	    loop(GruposActualizados);
    	
        Msg ->
    	    io:format("[~p] WTF? ~p~n", [?MODULE, Msg]),
    	    loop(Grupos)
    end.


%% Indica si un grupo existe o no, sin crearlo.
%% existe_grupo(NombreGrupo, Grupos) -> boolean()
existe_grupo(NombreGrupo, Grupos) ->
    case lists:keyfind(NombreGrupo, #infoGrupo.nombre, Grupos) of
        false -> false
      ; _ -> true
    end.

%% Devuelve el grupo indicado por NombreGrupo de la lista de grupos y en caso de no existir lo crea
%% salida: {InfoGrupo, Grupos}
getGrupo(NombreGrupo, Grupos) ->
    case lists:keyfind(NombreGrupo, #infoGrupo.nombre, Grupos) of
        InfoGrupo when is_record(InfoGrupo, infoGrupo) ->
            {InfoGrupo, Grupos}

        ; false ->
            PidNuevoGrupo = grupo:crear_y_enlazar(self()), % Se le pasa al constructor el pid del master
            NuevoGrupo = #infoGrupo{nombre=NombreGrupo, pid_grupo=PidNuevoGrupo},
            { NuevoGrupo, [NuevoGrupo | Grupos] }
    end.
