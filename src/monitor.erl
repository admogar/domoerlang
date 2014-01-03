%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Monitor of an automation component. Checks its state and,
%%% if that's the case, sends orders to it.
%%% @see master
%%% @see grupo
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
%%                  Type:: bin
%%                  | {num, Min :: integer(), Max :: integer()})
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

%%--------------------------------------------------------------------
%% @doc Configures a monitor in order to send notifications to a group
%% @spec configurar_padre(PidMonitor :: pid(),
%%                        PidGrupo :: pid())
%%                        -> ok
%% @end
%%--------------------------------------------------------------------
configurar_padre(PidMonitor, PidGrupo) ->
    PidMonitor ! {self(), {configurar_padre, PidGrupo}}.

%%--------------------------------------------------------------------
%% @doc Changes the monitor state to working
%% @spec start(PidMonitor :: pid()) -> starting
%% @end
%%--------------------------------------------------------------------
start(PidMonitor) ->
    PidMonitor ! {self(), start}.
    

%%% Internal implementation %%%

%%% ====================== Estado STOPPED ===============================
loop_stopped(GroupPid, State) ->
    receive
	{From, ping} ->
	    From ! {self(), pong},
	    loop_stopped(GroupPid, State);
	{From, upgrade} ->
	    From ! {self(), ok},
	    ?MODULE:init(GroupPid, State);
    	{From, {configurar_padre, NewGroupPid}} ->
	    From ! {self(), ok},
	    loop_stopped(NewGroupPid, State) ;
	{From, start} ->
	    From ! {self(), starting},
	    loop_started(GroupPid, State)
    end.

%%% ====================== Estado STARTED ===============================

loop_started(GroupPid, State) ->
% {GroupPid, Value}
% {GroupPid, true}

% {GroupPid, {Value, Min, Max}} *** Implicit type on Value
% {GroupPid, {5, 0, 10}}
    receive
	{From, ping} ->
	    From ! {self(), pong},
	    loop_stopped(GroupPid, State);
	{From, upgrade} ->
	    From ! {self(), upgrading},
	    ?MODULE:init(GroupPid, State);
	{From, getValue} ->
	    case State of
		{Value, _Min, _Max} -> % num
		    From ! {self(), Value};
		Value -> % bin
		    From ! {self(), Value}
	    end,
	    loop_started(GroupPid, State);
	{From, {setValue, NewValue}} ->
	    From ! {self(), NewValue},
	    case State of
		{_Value, Min, Max} ->
		    % TODO Percentual change notification function
		    GroupPid ! {self(), NewValue},
		    loop_started(GroupPid, {NewValue, Min, Max});
		_Value ->
		    GroupPid ! {self(), NewValue},
		    loop_started(GroupPid, NewValue)
	    end
    after
	?TIMEOUT ->
	    GroupPid ! {self(), heartbeat},
	    loop_started(GroupPid, State)
    end.
