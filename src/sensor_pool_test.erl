%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Sensor pool tests module
%%% @see sensor_pool
%%% @see sensor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sensor_pool_test).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

sensor_pool_test_() ->
    {setup,
     fun() -> try 
		  sensor_pool:start()
	      catch
		  {error, sensor_pool_already_running} ->
		      ok
	      end
     end,
     fun(_) ->
	     fun() ->
		     test_bin_("luz"),
		     test_num_("temperatura"),
		     test_unknown_("sensor")
	     end
     end}.

test_bin_(SensorName) ->
    ?_assertMatch({bin, _}, sensor_pool:get_sensor(SensorName)).

test_num_(SensorName) ->
    ?_assertMatch({num, _}, sensor_pool:get_sensor(SensorName)).

test_unknown_(SensorName) ->
    ?_assertThrow({error, sensor_no_existente}, sensor_pool:get_sensor(SensorName)).
