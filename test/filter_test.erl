-module(filter_test).
-compile(export_all).

before_suite() ->
	filter:start().

after_suite() ->
	filter:stop().

test_filter_chain() ->
	filter:new([1,25], [0.8], self()).
