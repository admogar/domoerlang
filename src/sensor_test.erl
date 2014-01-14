%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Sensor tests module
%%% @see sensor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sensor_test).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

bin_sensor_test_() ->
    fun() ->
	    SensorB = sensor:start(bin),
	    sensor:set_observer(SensorB),
	    receive
		{valor, ValueB} ->
		    ?assert(erlang:is_boolean(ValueB))
	    end,
	    exit(SensorB,hard)
    end.

num_sensor_test_() ->
    fun() ->
	    SensorN = sensor:start({num, 0, 10}),
	    sensor:set_observer(SensorN),
	    receive
		{valor, ValueN} ->
		    ?assert(erlang:is_integer(ValueN))
	    end,
	    exit(SensorN,hard)
    end.
