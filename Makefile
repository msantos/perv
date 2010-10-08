
REBAR=$(shell which rebar || echo ./rebar)

all: dirs deps compile

./rebar:
	erl -noshell -s inets start \
		-eval 'httpc:request(get, {"http://hg.basho.com/rebar/downloads/rebar", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

dirs:
	@mkdir -p priv/tmp priv/log priv/files/image

deps: $(REBAR)
	@$(REBAR) get-deps

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

