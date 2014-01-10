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

-export([anadir_sensor/2, obtener_grupos/0, obtener_estado_grupo/1, upgrade/0, version/0, ping/1]).

-export([loop/1]).
%%% Ver http://www.erlang.org/doc/reference_manual/records.html
%%% Ver http://www.erlang.org/doc/programming_examples/records.html
-record(infoGrupo, {nombre, sensores = [], pid_grupo}).

%%--------------------------------------------------------------------
%% @doc Starts the master.
%% @spec start() -> ok | exception
%% @end
%%--------------------------------------------------------------------
start() ->
    PidMaster = whereis(?MASTER),
    if
        PidMaster/=undefined ->
            throw({error, master_already_running})
      ; true ->
            spawn(fun() -> init() end)
    end.

%%--------------------------------------------------------------------
%% @doc Stops the master.
%% @spec stop() -> ok | timeout
%% @end
%%--------------------------------------------------------------------
stop() ->
    ?MASTER ! {self(), stop},
    receive
	{?MASTER, stopping} ->
	    ok
    after
	?TIMEOUT ->
	    timeout
    end.

anadir_sensor(NombreGrupo, IdSensor) ->
    ?MASTER ! {self(), {add, IdSensor, NombreGrupo}}.

%% Devuelve una lista con los nombres de grupos o [error]
obtener_grupos() ->
    ?MASTER ! {self(), get_lista_grupos},
    receive
        {grupos, ListaNombreGrupos} -> ListaNombreGrupos
    after
        ?TIMEOUT ->
            [error]
    end.


obtener_estado_grupo(NombreGrupo) ->
    ?MASTER ! {self(), get_estado_grupo, NombreGrupo},
    receive
        {estado_grupo, ListaEstados} -> ListaEstados
    after
        ?TIMEOUT ->
            timeout
    end.
    
upgrade() ->
    ?MASTER ! {self(), upgrade},
    receive
    {?MASTER, upgrading} ->
      ok
    after
    ?TIMEOUT ->
      timeout
    end.

version() ->
    ?MASTER ! {self(), version},
    receive
    {?MASTER, Version} ->
      Version
    after
    ?TIMEOUT ->
      timeout
    end.
    
ping(NombreGrupo) -> 
    ?MASTER ! {self(), ping, NombreGrupo},
    receive
        {?MASTER, pong} -> pong
    after
        ?TIMEOUT ->
            timeout
    end.
%%% Internal Implementation

init() ->
    register(?MASTER, self()),
    process_flag(trap_exit, true),
    loop([]).

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
            unregister(?MASTER),
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
