-module(ircbot_server).
-author('jrb').

-define(LINEFEED, "\r\n").
-define(TCP_OPTIONS, [binary, {packet, line}, {keepalive, true} ]).
-define(SSL_OPTIONS, [binary, {verify, 0}, {packet, line}]).



-behaviour(gen_server).

-export([start_link/0, connect/5, ssl_connect/6, join/2, say/3, pong/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([send_line/2, send_line/3]).

-record(conn_info, {socket, server, port, nickname, realname, channels = [], pid=self(), ssl=false}).

%%--------------
%%
%% external API
%%
%%--------------

start_link() ->
%%  irc_event_manager:start_link(),
%%  irc_event_manager:add_handler(irc_rawtext_handler),
  gen_server:start_link(?MODULE, [], []).
 
connect(Pid, Server, Port, Nick, Real) ->
	gen_server:call(Pid, {connect, [Server, Port, Nick, Real]}).

ssl_connect(Pid, Server, Port, Nick, Real, Pass) ->
	gen_server:call(Pid, {connect, [Server, Port, Nick, Real, {ssl, true}, {password, Pass}]}).

join(Pid, Channel) ->
	gen_server:call(Pid, {join, Channel}).


say(Pid, Focus, Msg) ->
	gen_server:call(Pid, {say, Focus, Msg}).

pong(Pid, Server) ->
	gen_server:call(Pid, {pong, Server}).


send_line(Socket, Message, {ssl, true}) ->
	io:fwrite("~w~n", [Message]),
	ssl:send(Socket, Message ++ ?LINEFEED);
send_line(Socket, Message, {ssl, false}) ->
	gen_tcp:send(Socket, Message ++ ?LINEFEED);

send_line(Socket, Message, _) ->
	gen_tcp:send(Socket, Message ++ ?LINEFEED).

send_line(Socket, Message) ->
	gen_tcp:send(Socket, Message ++ ?LINEFEED).
	
%%------------------------------
%%
%% gen_server callback functions
%% 
%%------------------------------
init(_Args) ->
%	irc_event_manager:add_handler(irc_rawtext_handler),
	State = #conn_info{},
	{ok, State}.

handle_call({getstate, _}, _From, State) ->

    {reply, ok, State};
handle_call({connect, [Server, Port, Nick, Real, {ssl, true}, {password, Pass}]}, _From, _State) ->
	ssl:start(),
	{ok, Sock} = ssl:connect(Server, Port, ?SSL_OPTIONS),
	NewState = #conn_info{socket=Sock, server=Server, port=Port, 
				nickname=Nick, realname=Real, channels=[], ssl=true, pid=self() },
	
	send_line(Sock, ["NICK ", Nick], {ssl, true}),
	send_line(Sock, ["PASS ", Pass], {ssl, true}),
	send_line(Sock, ["USER ", Nick, " 8 * :", Real], {ssl, true}),

	{reply, ok, NewState};
handle_call({connect, [Server, Port, Nick, Real]}, _From, _State) ->

	%% set our configuration_info record

	
	
	
	{ok, Sock} = gen_tcp:connect(Server, Port, ?TCP_OPTIONS),
	NewState = #conn_info{socket=Sock, server=Server, port=Port, 
				   nickname=Nick, realname=Real, channels=[], pid=self(), ssl=false},
	
	send_line(Sock, ["NICK ", Nick], {ssl, false}),
	send_line(Sock, ["USER ", Nick, " 8 * :", Real], {ssl, false}), 
        { reply, ok, NewState };

handle_call({disconnect, QuitMsg}, _From, State) ->
	
	send_line(State#conn_info.socket, ["QUIT ", QuitMsg], {ssl, State#conn_info.ssl}),
	%%close_conn(State#conn_info.socket, {ssl, State#conn_info.ssl}),
	NewState = #conn_info{},

	{ reply, ok, NewState };


handle_call({nick, Nick}, _From, State) ->
	%% TODO add event handler for Operation #433 for failed nicknames 
    NickString = "NICK " ++ Nick ++ " ",

    io:format("Changing nick to: ~p  State: ~p~n", [NickString, State]),
	send_line(State#conn_info.socket, ["NICK " ++ Nick ++ " "], {ssl, State#conn_info.ssl}),
	NewState = State#conn_info{nickname=Nick},

	{reply, ok, NewState};

handle_call({join, Channel }, _From, State) ->

	%% Note we do not update the channels list here 
	%% we'll need to add an event handler for operation 353 for a successful join

	send_line(State#conn_info.socket, ["JOIN ", Channel], {ssl, State#conn_info.ssl}),
	{reply, ok, State };

handle_call({pong, Server}, _From, State) ->
	send_line(State#conn_info.socket, ["PONG :", Server, ?LINEFEED], {ssl, State#conn_info.ssl}),
	{reply, ok, State};

handle_call({say, Focus, Msg}, _From, State) ->
    LineToSend = ["PRIVMSG ", Focus, " :", Msg, ?LINEFEED],
	send_line(State#conn_info.socket, LineToSend, {ssl, State#conn_info.ssl}),
	{reply, ok, State};


handle_call({raw, String}, _From, State) ->
	send_line(State#conn_info.socket, String, {ssl, State#conn_info.ssl}),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.


parse_sender(UnparsedSender) ->
    [Sender|_] = string:tokens(UnparsedSender, "!"),
    Sender.

handle_info({_, _Sock, <<$:, ServerBin/binary>>}, State) ->
	ServerMessage = binary_to_list(ServerBin),
	%% I copied this method of parsing server messages from manderlbot, it seems to be the easiest way.
	%% props to mickael remond <3
	
	BodyPos = string:chr(ServerMessage, $:),

	case BodyPos > 0 of
		true ->
			Command = string:tokens(string:substr(ServerMessage, 1, BodyPos - 2), " "),
	
			case length(Command) of
				1 ->
					From = lists:nth(1, Command),
					Operation = none,
					To = none,
					Options = none;
				2 ->
					From = lists:nth(1, Command),
					Operation = lists:nth(2, Command),
					To = none,
					Options = none;
				
				3 ->	
					From = lists:nth(1, Command),
					Operation = lists:nth(2, Command),
					To = lists:nth(3, Command),
					Options = none;
	
				_Other ->
                	                From = lists:nth(1, Command),
                        	        Operation = lists:nth(2, Command),
                                	To = lists:nth(3, Command),
					Options = lists:nthtail(3, Command)
		   	end,	


		    Message = string:substr(ServerMessage, BodyPos + 1),

		    irc_event_manager:notify([{parsed_msg, parse_sender(From), Operation, To, Options, Message}, {serverpid, self()}]),
		    {noreply, State};

		false ->
			%% Our command has no accompanying message so just parse as is
			Command = string:tokens(ServerMessage, " "),
			Operation = lists:nth(2, Command),
			From = lists:nth(1, Command),
			To = lists:nth(3, Command),
			irc_event_manager:notify([{parsed_msg, From, Operation, To}, {serverpid, self()}]),
			{noreply, State}
	end;	 
handle_info({_, _Sock, <<"PING :",  FromBin/binary>>}, State) ->
	%% PING/PONG responses are fairly low-level so we've implemented this here.  This does not need to be user editable and so should remain in this low level portion of the bot.

	From = binary_to_list(FromBin),
	send_line(State#conn_info.socket, ["PONG :", From, ?LINEFEED], {ssl, State#conn_info.ssl}),

	{noreply, State};

handle_info({_, _Sock, Data}, State) ->

	irc_event_manager:notify({raw_text,  Data}),
	{noreply, State};
		
	
handle_info(Info, State) ->
  io:fwrite("~w~n", [Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
