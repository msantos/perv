
REBAR=$(shell which rebar || echo ./rebar)

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

