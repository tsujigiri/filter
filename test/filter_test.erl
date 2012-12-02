-module(filter_test).
-compile(export_all).

before_suite() ->
	application:start(sasl),
	application:start(gproc),
	filter:start().

after_suite() ->
	filter:stop(),
	application:stop(gproc),
	application:stop(sasl).

test_iir_in_combination() ->
	Filter = filter:new([1], [0.8], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[0, 1.0, -0.8, 0.6400000000000001] = Out.

test_fir_in_combination() ->
	Filter = filter:new([1,2,3], [0], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[0, 1, 2, 3] = Out.

test_iir_only() ->
	Filter = filter:new([], [0.8], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[0, 1.0, -0.8, 0.6400000000000001] = Out.

test_fir_only() ->
	Filter = filter:new([1,2,3], [], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[0, 1, 2, 3] = Out.

