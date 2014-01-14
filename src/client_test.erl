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
		     {inorder,
		      [
		       client:add("cocina", "luz"),
		       client:add("cocina", "temperatura"),
		       test_listGroups([{"cocina", nonode@nohost}]),
		       test_check("cocina"),
		       test_ping("cocina"),
		       test_bin("cocina", "luz"),
		       test_num("cocina", "temperatura"),
		       ?assert(erlang:is_string(client:version())),
		       ?assertEqual(ok, client:upgrade())
		      ]}
	     end
     end}.

test_listGroups(ExpectedList) ->
    ?assertEqual(ExpectedList, client:listGroups()).

test_check(GroupName) ->
    ?assertMatch([{"temperatura", _, _}, {"luz", _, _}],
		  client:checkGroup(GroupName)).

test_bin(GroupName, SensorName) ->
    ?assert(erlang:is_boolean(client:getSensorValue(GroupName, SensorName))).

test_num(GroupName, SensorName) ->
    ?assert(erlang:is_integer(client:getSensorValue(GroupName, SensorName))).

test_ping(GroupName) ->
    ?assertMatch({_, pong}, client:ping(GroupName)).

setup() ->
    sensor_pool:start(),
    client:start().

teardown() ->
    client:stop().
