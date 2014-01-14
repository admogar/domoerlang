%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Client tests module
%%% @see client
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(client_test).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

client_test_() ->
    {setup,
     fun() -> setup() end,
     fun(_) -> teardown() end,
     fun(_) ->
	     fun() ->
		     client:add("cocina", "luz"),
		     client:add("cocina", "temperatura"),
		     test_listGroups([{"cocina", nonode@nohost}]),
		     test_check("cocina"),
		     test_ping("cocina"),
		     fun() -> {parallel,
		      [
		       test_bin("cocina", "luz"),
		       test_num("cocina", "temperatura")
		      ]
		     }
		     end
	     end
     end}.

test_listGroups(ExpectedList) ->
    ?assertEqual(ExpectedList, client:listGroups()).

test_check(GroupName) ->
    ?assertMatch([{"temperatura", _, _}, {"luz", _, _}],
		  client:checkGroup(GroupName)).

test_bin(GroupName, SensorName) ->
    timer:sleep(4000),
    ?assert(erlang:is_boolean(client:getSensorValue(GroupName, SensorName))).

test_num(GroupName, SensorName) ->
    timer:sleep(4000),
    ?assert(erlang:is_integer(client:getSensorValue(GroupName, SensorName))).

test_ping(GroupName) ->
    ?assertMatch({_, pong}, client:ping(GroupName)).

setup() ->
    try
	sensor_pool:start()
    catch
	{error, sensor_pool_already_running} ->
	    ok
    end,
    client:start().

teardown() ->
    client:stop().
