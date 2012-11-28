REBAR = rebar
.PHONY: test

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) -C rebar.test.config compile
	deps/etest/bin/etest-runner
