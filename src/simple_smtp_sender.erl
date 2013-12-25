%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Omar Álvarez <omar.alvarez@udc.es>
%%% @author Noelia Luaces <nluaces@gmail.com>
%%% @author Adrián Morán <admogar@gmail.com>
%%% @author Alfonso Nishikawa <alfonso.nishikawa@gmail.com>
%%% @author David Torres <davidtorresandreu@gmail.com>
%%% @doc Simple STMP Sender. Sends emails when necessary 
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(simple_smtp_sender).

-export([send/4, send/5, send/6, send/7]).

%%--------------------------------------------------------------------
%% @doc Sends an email with no especified server.
%% @spec send(To :: nonempty_string(),
%%            From :: nonempty_string(),
%             Subject :: nonempty_string(),
%%            Message :: nonempty_string())
%%           -> send(To, From, Subject, Message, Server)
%% @end
%%--------------------------------------------------------------------
send(To, From, Subject, Message)->
	Server="localhost",
	send(To, From, Subject, Message, Server).

%%--------------------------------------------------------------------
%% @doc Sends an email with no especified port number.
%% @spec send(To :: nonempty_string(),
%%            From :: nonempty_string(),
%             Subject :: nonempty_string(),
%%            Message :: nonempty_string(),
%%            Server :: nonempty_string())
%%           -> send(To, From, Subject, Message, Server, PortNo)
%% @end
%%--------------------------------------------------------------------
send(To, From, Subject, Message, Server)->
	PortNo=25,	
	send(To, From, Subject, Message, Server, PortNo).

%%--------------------------------------------------------------------
%% @doc Sends an email with no especified hostname.
%% @spec send(To :: nonempty_string(),
%%            From :: nonempty_string(),
%             Subject :: nonempty_string(),
%%            Message :: nonempty_string(),
%%            Server :: nonempty_string(),
%%            PortNo :: integer())
%%           -> send(To, From, Subject, Message, Server, PortNo, Hostname)
%% @end
%%--------------------------------------------------------------------
send(To, From, Subject, Message, Server, PortNo)->
	{ok, Hostname}=inet:gethostname(),
	send(To, From, Subject, Message, Server, PortNo, Hostname).

%%--------------------------------------------------------------------
%% @doc Sends an email.
%% @spec send(To :: nonempty_string(),
%%            From :: nonempty_string(),
%             Subject :: nonempty_string(),
%%            Message :: nonempty_string(),
%%            Server :: nonempty_string(),
%%            PortNo :: integer(),
%%            LocalDomain :: nonempty_string())
%%           -> ok
%% @end
%%--------------------------------------------------------------------
send(To, From, Subject, Message, Server, PortNo, LocalDomain)->
	{ok,Sock}=gen_tcp:connect(Server,PortNo,[{active, false}]),
	{ok, "220"++_}=gen_tcp:recv(Sock, 0),
	gen_tcp:send(Sock, lists:append(["HELO ", LocalDomain, [13, 10]])),
	{ok, "250"++_}=gen_tcp:recv(Sock, 0),
	gen_tcp:send(Sock, lists:append(["MAIL FROM: ", From, [13, 10]])),
	{ok, "250"++_}=gen_tcp:recv(Sock, 0),
	gen_tcp:send(Sock, lists:append(["RCPT TO: ", To, [13, 10]])),
	{ok, "250"++_}=gen_tcp:recv(Sock, 0),
	gen_tcp:send(Sock, lists:append(["DATA", [13, 10]])),
	{ok, "354"++_}=gen_tcp:recv(Sock, 0),
	gen_tcp:send(Sock, lists:append(["Subject: ", Subject, [13, 10]])),
	gen_tcp:send(Sock, lists:append(["Mime-Version: 1.0;",[13, 10]])),%%Para poner HTML en emails
	gen_tcp:send(Sock, lists:append(["Content-Type: text/html;",[13, 10],[13, 10]])),
	gen_tcp:send(Sock, lists:append([Message])),
	gen_tcp:send(Sock, lists:append([[13, 10],".",[13, 10],[13, 10]])),
	{ok, "250"++_}=gen_tcp:recv(Sock, 0),
	%gen_tcp:send(Sock,lists:append("QUIT", [13, 10])), %%Comentado porque nuestro server SMTP no lo soporta
	%{ok, "221"++_}=gen_tcp:recv(Sock, 0),
	gen_tcp:close(Sock),
	ok.	
