%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar �lvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adri�n Mor�n <admogar@gmail.com>
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
%%% (el enlazamiento depender� de qui�n est� observando, no al rev�s).
%%% 
%%% Para ello se dispone de la funci�n {@link crear_grupo/0}
%%% Por cuestiones de comodidad se dispone tambi�n de la funcion {@link crear_y_enlazar/0} 
%%%
%%% === A�adir monitor al grupo ===
%%% Para a�adir un monitor se dispone de la funci�n {@link a�adir_monitor/1}
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
    spawn(?MODULE, fun() -> init() end ,[]).

crear_y_enlazar() ->
    spawn_link(?MODULE, fun() -> init() end , []).
    

%% ====================================================================
%% Internal functions
%% ====================================================================

% Ver documentaci�n sobre conjuntos (m�dulo 'sets'): http://www.erlang.org/doc/man/sets.html


init() ->
    process_flag(trap_exit, true),
    loop(sets:new()). % Lista vac�a de monitores


% Monitores :: set(PidMonitor,...)
loop(Monitores) ->
    receive

        {_From, {anadir_monitor, PidMonitor}} ->
            % Si ya contenemos el monitor, ignoramos
            case sets:is_element(PidMonitor, Monitores) of
                true -> loop(Monitores)
                ; false ->
                    % Si no contenemos el monitor lo a�adimos y configuramos
                    NuevosMonitores = sets:add_element(PidMonitor, Monitores),
                    monitor:configurar_padre(PidMonitor, self()), % Monitor notificar� a self

                    loop(NuevosMonitores)
            end
        ; _ -> false
    end.





