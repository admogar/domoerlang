%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Monitor tests module
%%% @see monitor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(monitor_test).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

monitor_test_() ->
    {setup,
     fun() ->
	     sensor_pool:start(),
	     master:start()
     end,
     fun(_) -> master:stop() end,
     fun(_) ->
	     fun() ->
		     Monitor = monitor:start_link(self(), "luz"),
		     monitor:configurar_padre(Monitor,self()),
		     monitor:start(Monitor),
		     receive_monitor_heartbeat(),
		     receive_monitor_value(),
		     monitor:pause(Monitor)
	     end
     end}.

receive_monitor_heartbeat() ->
    receive
	X ->
	    ?assertMatch({_Monitor, heartbeat}, X)
    end.

receive_monitor_value() ->
    receive
	{_Pid, Value} ->
	    ?assert(erlang:is_boolean(Value))
    end.
