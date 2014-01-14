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
	     try
		 sensor_pool:start()
	     catch
		 {error, sensor_pool_already_running} ->
		     ok
	     end,
	     master:start()
     end,
     fun(_) -> master:stop() end,
     fun(_) ->
	     fun() ->
		     Monitor = monitor:start_link(self(), "luz"),
		     monitor:start(Monitor),
		     monitor:configurar_padre(Monitor,self()),
		     timer:sleep(4000),
		     receive_monitor_heartbeat(),
		     receive_monitor_value(Monitor),
		     monitor:pause(Monitor)
	     end
     end}.

receive_monitor_heartbeat() ->
    receive
	X ->
	    ?assertMatch({_Monitor, heartbeat}, X)
    end.

receive_monitor_value(Monitor) ->
    Monitor ! {self(), getValue},
    receive
	{_Pid, Value} ->
	    ?assert(erlang:is_boolean(Value))
    end.
