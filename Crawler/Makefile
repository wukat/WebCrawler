FLAGS=-pa ./ebin -pa ./deps/*/ebin

REBAR=./rebar

all: deps compile

clean:
	@$(REBAR) clean

$(REBAR):
	wget https://github.com/rebar/rebar/wiki/rebar
	chmod a+x rebar

deps:
	$(REBAR) get-deps
	$(REBAR) compile

get-deps: $(REBAR)
	$(REBAR) get-deps
	$(REBAR) compile

compile: $(REBAR) deps
	$(REBAR) compile

run: compile
	exec erl $(FLAGS) -s crawlerApplication standard_start

run_only_deps: compile
	exec erl $(FLAGS) -s crawlerApplication standard_start

clean-deps: clean
	rm -rvf ./deps/*

rebuild: clean-deps get-deps all
