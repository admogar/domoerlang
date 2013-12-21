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
-export([start/0, init/1]).

%%--------------------------------------------------------------------
%% @doc Starts the master.
%% @spec start() -> ok | exception
%% @end
%%--------------------------------------------------------------------
start() ->
    spawn(fun() -> init() end),
    ok.

%%--------------------------------------------------------------------
%% @doc Inits or continues the master's execution.
%% @end
%%--------------------------------------------------------------------
init(Monitors) ->
    loop(Monitors).

%%% Internal Implementation

init() ->
    register(?MASTER, self()),
    process_flag(trap_exit, true),
    init([]).

%% {From, {add, Monitor}} - Anadir un monitor
%% {From, {list_monitors}} - Lista de monitores en master
%% {From, {check,Monitor}} - Comprobar estado monitor
%% {From, {version}} - Version de master
%% {From, {upgrade}} - Actualizar master en caliente
%% {From, {stop}} - Parar master
%% {'EXIT', Pid, Reason} - En caso de error en monitor, reinicio
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
    {From, {check,Monitor}} ->
      case lists:keyfind(Monitor, 1, Monitors) of
	 {Monitor, MonitorProc} ->
        %From ! {?MASTER, MonitorProc},
        MonitorProc ! {?MASTER, {ping}},
        receive
          {MonitorProc, {pong,Value}} ->
            From ! {?MASTER, Value}
        after ?TIMEOUT ->
            timeout
        end;
      false ->
        io:format("Monitor *~p* does not exist~n",[Monitor])
      end,
      loop(Monitors);
    {From, {version}} ->
      From ! {?MASTER, ?VERSION},
      loop(Monitors);
    {From, {upgrade}} ->
      From ! {?MASTER,ok},
      ?MODULE:init(Monitors);
    {From, {stop}} ->
      From ! {?MASTER, stopping};
    {'EXIT', Pid, Reason} -> 
      io:format("Got exit signal from ~p: ~p~n", [Pid,Reason]),
      {Monitor, MonitorProc} = lists:keyfind(Pid, 2, Monitors),
      TempMons = lists:keydelete(Pid, 2, Monitors),
      io:format("Error in monitor ~p with process ~p~n", [Monitor,MonitorProc]),
      {_MonitorNewProc, NewMonitors} = getMonitor(Monitor, TempMons),
      io:format("Monitor restarted!~n"),
      simple_smtp_sender:send(?ADMIN_MAIL, ?DOMOERL_MAIL, "Fallo en domoerlang", io_lib:format("<!DOCTYPE html><html><body>El proceso ~p encargado de <strong>~p</strong> se ha petado debido a <strong>~p</strong> y ha habido que reiniciarlo.~n <p></p><img src=\"http://galeri3.uludagsozluk.com/138/facepalm_227785.jpg\" alt=\"Facepalm\"></body></html>", [Pid, Monitor, Reason]),  ?SMTP_SERV, ?SMTP_PORT),
      loop(NewMonitors);
    Msg ->
	 io:format("[~p] WTF? ~p~n", [?MODULE, Msg]),
      loop(Monitors)
  end.


%% Creacion de monitor, en caso de que exista no creamos uno nuevo
getMonitor(Monitor, Monitors) ->
    case lists:keyfind(Monitor, 1, Monitors) of
	{Monitor, MonitorProc} ->
	    {MonitorProc, Monitors};
    false ->
	    NewMonitor = monitor:start_link(),
	    {NewMonitor, [{Monitor,NewMonitor} | Monitors]}
    end.
