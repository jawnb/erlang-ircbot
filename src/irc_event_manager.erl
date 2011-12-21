-module(irc_event_manager).
-author("jrb").

-export([start_link/0, add_handler/1, notify/1]).

%% Our external API

start_link() ->

	io:format("IRC Event Manager start_link called. ~n", []),
	gen_event:start_link({local, ?MODULE}).

add_handler(Module) ->
	gen_event:add_handler(?MODULE, Module, []).

notify(Event) ->
	gen_event:notify(?MODULE, Event).


