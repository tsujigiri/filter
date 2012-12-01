-module(filter_test).
-compile(export_all).

before_suite() ->
	filter:start().

after_suite() ->
	filter:stop().

test_filter_chain() ->
	Filter = filter:new([1, 1.25], [0.8], self()),
	test_helper:filter_in(Filter, [0, 0.7, 1, 0.7, 0, -0.7, -1, -0.7]),
	Out = test_helper:filter_out(Filter, 8, []),
	[] = Out.
