#
#  erlbot Makefile
#
all:
	erlc -o ./ebin src/irc_event_manager.erl  src/irc_rawtext_handler.erl  src/ircbot.erl  src/ircbot_server.erl  src/ircbot_supervisor.erl

