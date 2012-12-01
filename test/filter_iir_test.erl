-module(filter_iir_test).
-compile(export_all).

before_suite() ->
	{ok, Pid} = filter:start_link(iir, [1,1], self()),
	register(filter_iir_test, Pid).

test_filter_run() ->
	Pid = whereis(filter_iir_test),
	ok = test_helper:filter_in(Pid, [1, 0.7, 0, -0.7, -1, -0.7, 0, 0.7]),
	[0, 1, 0.7, -1, -2.4, -0.7, 2.7, 3.0999999999999996] = test_helper:filter_out(Pid, 8, []).

