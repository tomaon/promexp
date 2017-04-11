#
 ERLANG_HOME ?= /opt/erlang/release/latest

 REBAR ?= ./rebar3

 ENV  =
 ENV += ERL_FLAGS="+A 10"
 ENV += REBAR_CONFIG=rebar3.config
 ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
#ENV += DEBUG=1

 WORK = .rebar3

#
default: compile

#
$(VERBOSE).SILENT:

all: build

build:
	$(ENV) $(REBAR) as prod compile

compile ct dialyzer eunit shell:
	$(ENV) $(REBAR) as test $@

clean: rm
	for P in prod test; do $(ENV) $(REBAR) as $$P clean; done
cleanall: rm
	for P in prod test; do $(ENV) $(REBAR) as $$P clean --all; done
distclean: rm
	rm -rf $(WORK)

rm: rm-autosave rm-dump rm-logs

rm-autosave:
	find . -name "*~" | xargs rm -f
rm-dump:
	rm -f erl_crash.dump
rm-logs:
	for D in cover logs; do rm -rf $(WORK)/test/$$D; done

test: rm-logs ct


elvis:
	elvis rock

#
run: build
	$(ENV) ERL_LIBS=$(WORK)/prod/lib erl -config examples/inets -s promexp -s inets 
