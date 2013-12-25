%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar √Ålvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adri√°n Mor√°n <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Monitor of an automation component. Checks its state and,
%%% if that's the case, sends orders to it.
%%% @see master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(monitor).

-include("client.hrl").

%% PUBLIC API
-export([start_link/2, init/2]).

-export([configurar_padre/2 , start/1]).

%%--------------------------------------------------------------------
%% @doc Starts the monitor.
%% @spec start_link(GroupPid :: pid(),
%%                  Type:: atom()
%%                  | {atom, Min :: integer(), Max :: integer()})
%%                  -> ok | exception
%% @end
%%--------------------------------------------------------------------
start_link(GroupPid, Type) -> % Type : bin | {num, Min, Max}
    case Type of
	bin ->
	    spawn_link(fun() -> init(GroupPid, false) end);
	{num, Min, Max} ->
	    spawn_link(fun() -> init(GroupPid, {Min, Min, Max}) end)
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Inits or continues the monitor's execution.
%% @end
%%--------------------------------------------------------------------
init(GroupPid, State) ->
    loop_stopped(GroupPid, State).


%% Configura un Monitor para que envÌe las notificaciones a un Grupo
configurar_padre(PidMonitor, PidGrupo) ->
    PidMonitor ! {self(), {configurar_padre, PidGrupo}}.

%% Inicia el funcionamiento del monitor (transiciÛn del estado a 'start')
start(PidMonitor) ->
    PidMonitor ! {self(), {start}}.
    

%%% Internal implementation %%%

%%% ====================== Estado STOPPED ===============================
loop_stopped(GroupPid,State) ->
    receive
    {From, ping} ->
        From ! {self(), pong} ;
        
    {From, upgrade} ->
        From ! {self(), ok},
        ?MODULE:init(GroupPid, State);
        
    {_From, {configurar_padre, NuevoPidGrupo}} ->
        loop_stopped(NuevoPidGrupo, State) ;

    {_From, {start}} ->
        loop_started(GroupPid, State)
    
    end.

%%% ====================== Estado STARTED ===============================

loop_started(GroupPid, State) ->
% {GroupPid, Value}
% {GroupPid, true}

% {GroupPid, {Value, Min, Max}} *** Implicit type on Value
% {GroupPid, 5, {0, 10}}
    receive
	{From, ping} ->
	    From ! {self(), pong} ;
        
	{From, upgrade} ->
	    From ! {self(), ok},
	    ?MODULE:init(GroupPid, State);
        
	{From, getValue} ->
	    case State of
		{Value, _Min, _Max} -> % num
		    From ! {self(), Value},
		    loop_started(GroupPid, State);
		Value -> % bin
		    From ! {self(), Value},
		    loop_started(GroupPid, State);
		false ->
		    From ! badArgument, % FIXME
		    loop_started(GroupPid, State)
	    end;
        
	{From, {setValue, NewValue}} ->
	    case State of
		{_Value, Min, Max} ->
		    loop_started(GroupPid, {NewValue, Min, Max});
		_Value ->
		    GroupPid ! {self(), NewValue},
		    loop_started(GroupPid, NewValue);
		false ->
		    From ! error, % FIXME
		    loop_started(GroupPid, State)
	    end

    after ?TIMEOUT ->
	    GroupPid ! {self(), heartbeat},
	    loop_started(GroupPid, State)
    end.

% TODO Percentual change function
