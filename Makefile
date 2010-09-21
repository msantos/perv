
REBAR=$(shell which rebar || echo ./rebar)

all: dirs deps compile

dirs:
	@mkdir -p priv/tmp priv/log priv/files/image

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

