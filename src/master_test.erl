%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Master tests module
%%% @see master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(master_test).

-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

master_test_() ->
    {setup,
     fun() ->
	     try
		 sensor_pool:start()
	     catch
		 {error, sensor_pool_already_running} ->
		     ok
	     end,
	     master:start() end,
     fun(_) -> master:stop() end,
     fun(_) ->
	     fun() ->
		     test_singrupos(),
		     test_nogrupo(),
		     test_congrupo(),
		     test_estadogrupo(),
		     test_valorsensor(),
		     test_ping()
	     end
     end}.

test_singrupos() ->
    ?assertEqual([], master:obtener_grupos()).

test_nogrupo() ->
    ?assertEqual({error,grupo_inexistente},
		 master:obtener_valor_sensor("cocina", "luz")).

test_congrupo() ->
    master:anadir_sensor("cocina","luz"),
    ?assertEqual([{"cocina", nonode@nohost}],master:obtener_grupos()).
    
test_estadogrupo() ->
    ?assertMatch([{"luz",_,_}], master:obtener_estado_grupo("cocina")).

test_valorsensor() ->
    timer:sleep(4000),
    ?assert(erlang:is_boolean(master:obtener_valor_sensor("cocina", "luz"))).

test_ping() ->
    ?assertEqual(pong, master:ping("cocina")).
