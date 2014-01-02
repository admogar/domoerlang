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

%% ====================================================================
%% API functions
%% ====================================================================
-export([crear_grupo/0, crear_y_enlazar/0]).


crear_grupo() ->
    spawn(?MODULE, fun() -> init() end , []).

crear_y_enlazar() ->
    spawn_link(?MODULE, fun() -> init() end , []).
    

%% ====================================================================
%% Internal functions
%% ====================================================================

% Ver documentación sobre conjuntos (módulo 'sets'): http://www.erlang.org/doc/man/sets.html


init() ->
    process_flag(trap_exit, true),
    loop(sets:new()). % Lista vacía de monitores


% Monitores :: set(PidMonitor,...)
loop(Monitores) ->
    receive

        {_From, {anadir_monitor, PidMonitor}} ->
            % Si ya contenemos el monitor, ignoramos
            case sets:is_element(PidMonitor, Monitores) of
                true -> loop(Monitores)
                ; false ->
                    % Si no contenemos el monitor lo añadimos y configuramos
                    NuevosMonitores = sets:add_element(PidMonitor, Monitores),
                    monitor:configurar_padre(PidMonitor, self()), % Monitor notificará a self

                    loop(NuevosMonitores)
            end
        ; _ -> false
    end.





