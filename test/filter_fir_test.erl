-module(filter_fir_test).
-compile(export_all).


% tests

before_suite() ->
	{ok, Pid} = filter_fir:start_link([1,2,3], self()),
	register(test_filter_fir, Pid).

test_filter_run() ->
	ok = filter_in([1, 0.7, 0, -0.7, -1, -0.7, 0, 0.7]),
	[0, 1, 2.7, 4.4, 1.3999999999999997, -2.4, -4.8, -4.4] = filter_out(8, []).


% internal

filter_in([]) -> ok;

filter_in([Value | Values]) ->
	Pid = whereis(test_filter_fir),
	filter_fir:in(Pid, Value),
	filter_in(Values).

filter_out(0, Values) ->
	lists:reverse(Values);

filter_out(ValueCount, Values) ->
	Pid = whereis(test_filter_fir),
	filter_out(ValueCount-1, [filter_fir:out(Pid) | Values]).
