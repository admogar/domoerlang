%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Group tests module
%%% @see grupo
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(grupo_test).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

grupo_test_() ->
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
		     Master = global:whereis_name(master),
		     Grupo = grupo:crear(Master),
		     grupo:anadir_sensor(Grupo, "luz"),
		     ?assertMatch([{"luz",_,_}], grupo:obtener_estado(Grupo)),
		     timer:sleep(4000),
		     ?assert(erlang:is_boolean(grupo:obtener_valor_sensor(Grupo, "luz"))),
		     ?assertEqual(pong, grupo:ping(Grupo))
	     end
     end}.
