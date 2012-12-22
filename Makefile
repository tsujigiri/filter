PROJECT = filter
DIALYZER = dialyzer
REBAR = rebar
.PHONY: test

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

deps:
	$(REBAR) get-deps

test:
	$(REBAR) -C rebar.test.config compile
	deps/etest/bin/etest-runner

build-plt:
	$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl ./deps/gproc/ebin

dialyze: clean deps all
	$(DIALYZER) --plt .$(PROJECT).plt ebin

