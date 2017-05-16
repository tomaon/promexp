#
 ELIXIR_HOME ?= /opt/elixir/release/latest
 ERLANG_HOME ?= /opt/erlang/release/latest

#IEX ?= iex
 MIX ?= mix
 REBAR ?= rebar3

 ENV  =
#ENV += MIX_ENV=prod
 ENV += PATH=$(ELIXIR_HOME)/bin:$(ERLANG_HOME)/bin:$(PATH)

 WORK = .mix .rebar3

#
default: compile

#
.PHONY: test

$(VERBOSE).SILENT:

all: deps.get compile

#run: compile
#	$(ENV) $(IEX) -S mix

compile deps.get test:
	$(ENV) $(MIX) $@
clean: rm-autosave
	$(ENV) $(MIX) $@

dialyzer:
	$(ENV) $(REBAR) $@

distclean: rm-deps rm-doc rm-mix.lock
	rm -rf $(WORK)

rm-autosave:
	find . -name "*~" | xargs rm -f
rm-%:
	rm -rf $*

#
run:
	$(ENV) ERL_LIBS=$(firstword $(WORK))/dev/lib erl -config examples/inets -s promexp -s inets
