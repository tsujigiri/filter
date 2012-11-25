.PHONY: test

test:
	rebar compile
	deps/etest/bin/etest-runner
