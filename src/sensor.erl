%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Sensor emulator. Acts like a real component.
%%% @see monitor
%%% @see master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sensor).
-include("client.hrl").

%% PUBLIC API
-define(VERSION,1).
-export([start/0]).
-export([init/1]).

%%--------------------------------------------------------------------
%% @doc Starts the sensor.
%% @spec start() -> ok | exception
%% @end
%%--------------------------------------------------------------------
start() ->
    spawn(fun() -> init() end),
    ok.

%%--------------------------------------------------------------------
%% @doc Inits or continues the sensor's execution.
%% @end
%%--------------------------------------------------------------------
init(Monitors) ->
    loop(Monitors).

%%% Internal Implementation

init() ->
    %register(?MASTER, self()),
    %process_flag(trap_exit, true),
    init([]).

loop(Monitors) ->
    ok.
