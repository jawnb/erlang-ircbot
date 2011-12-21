-module(ircbot).
-author(jrb).

-behaviour(application).

-export([start_bot/0, stop/1, start/2]).
-export([connect/1, test/0]).


start_bot() ->
    start("X", "Y"),
    test().


start(_Type, _StartArgs) ->
	case ircbot_supervisor:start_link() of
		{ok, Pid} ->
			{ok, Pid};
	Error ->
		Error
	end.

connect([Server, Port, Nick, Real, ServerName]) ->
	ircbot_supervisor:add_server([Server, Port, Nick, Real, ServerName]).


test() ->
	Nick = "neckbeard",
	Real = "erlang lol",
    irc_event_manager:add_handler(irc_rawtext_handler),	
	connect(["irc.freenode.net", 6667, Nick, Real, "freenode"]).	

stop(_State)->
	exit(whereis(eb_sup), shutdown).

	
