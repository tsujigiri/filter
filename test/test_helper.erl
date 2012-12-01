-module(test_helper).
-compile(export_all).

filter_in(_Filter, []) -> ok;

filter_in(Filter, [Value | Values]) ->
	filter:in(Filter, Value),
	filter_in(Filter, Values).

filter_out(_Filter, 0, Values) ->
	lists:reverse(Values);

filter_out(Filter, ValueCount, Values) ->
	Value = filter:out(Filter),
	filter_out(Filter, ValueCount-1, [Value | Values]).
