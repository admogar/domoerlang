-module(master).
-include("client.hrl").

-define(VERSION,1).

-export([start/0,loop/1]).

start() ->
    spawn(fun() -> init() end),
    ok.

init() ->
    register(?MASTER, self()), 
    process_flag(trap_exit, true),
    loop([]).

loop(Monitors) ->
  receive
    {From, {add, Monitor}} ->
      {MonitorProc, NewMonitors} = getMonitor(Monitor, Monitors),
      %io:format(" WTF? ~n"),
      From ! {?MASTER, MonitorProc},
      loop(NewMonitors);
    {From, {list_monitors}} ->
      From ! {?MASTER, Monitors},
      loop(Monitors);
    {From, {version}} ->
      From ! {?MASTER, ?VERSION},
      loop(Monitors);
    {From, {upgrade}} ->
      From ! {?MASTER,ok},
      ?MODULE:loop(Monitors);
    {From, {stop}} ->
      From ! {?MASTER, stopping};
    {'EXIT', Pid, Reason} -> 
      io:format("Got exit signal from ~p: ~p~n", [Pid,Reason]),
      {Monitor, MonitorProc} = lists:keyfind(Pid, 2, Monitors),
      TempMons = lists:keydelete(Pid, 2, Monitors),
      io:format("Error in monitor ~p with process ~p~n", [Monitor,MonitorProc]),
      {MonitorNewProc, NewMonitors} = getMonitor(Monitor, TempMons),
      io:format("Monitor restarted!~n"),
      loop(NewMonitors);
    Msg ->
	 io:format("[~p] WTF? ~p~n", [?MODULE, Msg]),
      loop(Monitors)
  end.

getMonitor(Monitor, Monitors) ->
    case lists:keyfind(Monitor, 1, Monitors) of
	{Monitor, MonitorProc} ->
	    {MonitorProc, Monitors};
    false ->
	    NewMonitor = monitor:start_link(),
	    {NewMonitor, [{Monitor,NewMonitor} | Monitors]}
    end.
