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
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(monitor).

-include("client.hrl").

%% PUBLIC API
-export([start_link/2, init/2]).

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
    loop(GroupPid, State).

%%% Internal implementation %%%

loop(GroupPid, State) ->
% {GroupPid, Value}
% {GroupPid, true}

% {GroupPid, {Value, Min, Max}} *** Implicit type on Value
% {GroupPid, 5, {0, 10}}
    receive
	{From, ping} ->
	    From ! {self(), pong};
	{From, upgrade} ->
	    From ! {self(), ok},
	    ?MODULE:init(GroupPid, State);
	{From, getValue} ->
	    case State of
		{Value, Min, Max} -> % num
		    From ! {self(), Value},
		    loop(GroupPid, State);
		Value -> % bin
		    From ! {self(), Value},
		    loop(GroupPid, State);
		false ->
		    From ! badArgument, % FIXME
		    loop(GroupPid, State)
	    end;
	{From, {setValue, NewValue}} ->
	    case State of
		{_Value, Min, Max} ->
		    loop(GroupPid, {NewValue, Min, Max});
		_Value ->
		    GroupPid ! {self(), NewValue},
		    loop(GroupPid, NewValue);
		false ->
		    From ! error, % FIXME
		    loop(GroupPid, State)
	    end
    after ?TIMEOUT ->
	    GroupPid ! {self(), heartbeat},
	    loop(GroupPid, State)
    end.

% TODO Percentual change function
