-module(filter_test).
-compile(export_all).

before_suite() ->
	application:start(sasl),
	application:start(gproc),
	application:start(filter).

after_suite() ->
	application:stop(filter),
	application:stop(gproc),
	application:stop(sasl).

test_fir_in_combination() ->
	Filter = filter:new([1,2,3], [0], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[1, 2, 3, 0] = Out.

test_fir_only() ->
	Filter = filter:new([1,2,3], [], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[1, 2, 3, 0] = Out.

test_iir_in_combination() ->
	Filter = filter:new([1], [0.8], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[1.0, -0.8, 0.6400000000000001, -0.5120000000000001] = Out.

test_iir_only() ->
	Filter = filter:new([], [0.8], self()),
	test_helper:filter_in(Filter, [1,0,0,0]),
	Out = test_helper:filter_out(Filter, 4, []),
	[1.0, -0.8, 0.6400000000000001, -0.5120000000000001] = Out.

