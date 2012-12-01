-module(filter_fir_test).
-compile(export_all).

before_suite() ->
	Ref = make_ref(),
	{ok, Pid} = filter:start_link(fir, Ref, [1,2,3], self()),
	register(test_filter_fir, Pid).

test_filter_run() ->
	Filter = whereis(test_filter_fir),
	ok = test_helper:filter_in(Pid, [1, 0.7, 0, -0.7, -1, -0.7, 0, 0.7]),
	[0, 1, 2.7, 4.4, 1.3999999999999997, -2.4, -4.8, -4.4] = test_helper:filter_out(Pid, 8, []).

