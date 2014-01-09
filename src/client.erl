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
-export([add/2, listMonitors/1, checkMonitor/2]).
-export([upgrade/1, version/1]).

%%--------------------------------------------------------------------
%% @doc Commands the master to start.
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    ?MASTER:start(),
    ok.

%%--------------------------------------------------------------------
%% @doc Commands the master to stop.
%% @spec stop(MasterNode :: pid()) ->
%%            State :: atom() |
%%                     timeout 
%% @end
%%--------------------------------------------------------------------
stop(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), stop},
    receive
	{?MASTER, State} ->
	    State
    after
	?TIMEOUT ->
	    timeout
    end.

%%--------------------------------------------------------------------
%% @doc Adds a new monitor.
%% @spec add(MasterNode :: pid(), Monitor :: pid()) ->
%%           State :: pid() | timeout
%% @end
%%--------------------------------------------------------------------
add(MasterNode, Monitor) ->
    {?MASTER, MasterNode} ! {self(), {add, Monitor}},
    receive
	{?MASTER, State} ->
	    State
    after
	?TIMEOUT ->
	    timeout
    end.
    
%%--------------------------------------------------------------------
%% @doc Lists active monitors.
%% @spec listMonitors(MasterNode :: pid()) ->
%%                    list(pid()) | timeout
%% @end
%%--------------------------------------------------------------------
listMonitors(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), list_monitors},
    receive
	{?MASTER, List} ->
	    List
    after
	?TIMEOUT ->
	    timeout
    end.
    
%%--------------------------------------------------------------------
%% @doc Checks state of a sensor.
%% @spec checkMonitor(MasterNode :: pid(), Monitor :: pid()) ->
%%                    Value :: integer() | timeout
%% @end
%%--------------------------------------------------------------------
checkMonitor(MasterNode, Monitor) ->
    {?MASTER, MasterNode} ! {self(), {check, Monitor}},
    receive
	{?MASTER, Value} ->
	    Value
    after
	?TIMEOUT ->
	    timeout
    end.

%%--------------------------------------------------------------------
%% @doc Updates the server to the last compiled version.
%% @spec upgrade(MasterNode :: pid()) -> ok | timeout
%% @end
%%--------------------------------------------------------------------
upgrade(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), upgrade},
    receive
	{?MASTER, upgrading} ->
	    ok
    after
	?TIMEOUT ->
	    timeout
    end.

%%--------------------------------------------------------------------
%% @doc Prints the version on execution.
%% @spec version(pid()) ->
%%               nonempty_string() |
%%               timeout
%% @end
%%--------------------------------------------------------------------
version(MasterNode) ->
    {?MASTER, MasterNode} ! {self(), version},
    receive
	{?MASTER, Version} ->
	    Version
    after
	?TIMEOUT ->
	    timeout
    end.

%%% Internal Implementation
