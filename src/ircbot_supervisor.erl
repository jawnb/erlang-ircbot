-module(ircbot_supervisor).
-author(jrb).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, add_server/1]).
-export([add_ssl_server/1]).
-define(TCP_OPTIONS, [binary, {packet, line}, {keepalive, true} ]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
	EventManager = {irc_event_manager, {irc_event_manager, start_link, []},
			permanent,2000,worker,dynamic},


	{ok, {{one_for_one, 5, 10}, [EventManager]}}.

add_server([Server, Port, Nick, Real, ServerName]) ->
    {ok, ServerPid} = add_plaintext_server([Server, Port, Nick, Real, ServerName]),
    register(list_to_atom(ServerName), ServerPid),
    {ok, ServerPid}.


add_ssl_server([Server, Port, Nick, Real, ServerName, Password]) ->

       ServerAtom = list_to_atom(ServerName),
       ChildRef = {ServerAtom, {ircbot_server, start_link, []}, temporary, 2000, worker, [gen_server]},

        supervisor:start_child(ircbot_supervisor, ChildRef),

        Children = supervisor:which_children(ircbot_supervisor),

        {ServerAtom, ServerPid, _, _}  = lists:keyfind(ServerAtom, 1, Children),
        ircbot_server:ssl_connect(ServerPid, Server, Port, Nick, Real, Password),
        {ok, ServerPid}.

add_plaintext_server([Server, Port, Nick, Real, ServerName]) ->
	%% Add a new server process to the supervision tree and connect

	%% Grab our socket

	ServerAtom = list_to_atom(ServerName),


	ChildRef = {ServerAtom, {ircbot_server, start_link, []}, temporary, 2000, worker, [gen_server]},

	supervisor:start_child(ircbot_supervisor, ChildRef),
	
	Children = supervisor:which_children(ircbot_supervisor),

	{ServerAtom, ServerPid, _, _}  = lists:keyfind(ServerAtom, 1, Children),
	ircbot_server:connect(ServerPid, Server, Port, Nick, Real),
	{ok, ServerPid}.
		

