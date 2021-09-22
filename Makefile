REBAR = ./rebar3

all: compile ct shell

compile:
	$(REBAR) compile

ct:
	$(REBAR) ct

shell:
	$(REBAR) shell