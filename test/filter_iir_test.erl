-module(filter_iir_test).
-compile(export_all).

% tests

before_suite() ->
	{ok, Pid} = filter_iir:start_link([1,1], self()),
	register(test_filter_iir, Pid).

test_filter_run() ->
	ok = filter_in([1, 0.7, 0, -0.7, -1, -0.7, 0, 0.7]),
	[0, 1, 0.7, -1, -2.4, -0.7, 2.7, 3.0999999999999996] = filter_out(8, []).


% internal

filter_in([]) -> ok;

filter_in([Value | Values]) ->
	Pid = whereis(test_filter_iir),
	filter_iir:in(Pid, Value),
	filter_in(Values).

filter_out(0, Values) ->
	lists:reverse(Values);

filter_out(ValueCount, Values) ->
	Pid = whereis(test_filter_iir),
	filter_out(ValueCount-1, [filter_iir:out(Pid) | Values]).
