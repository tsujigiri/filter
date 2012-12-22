-module(filter).
-behaviour(gen_server).

%% api
-export([new/3, start_link/4, in/2, out/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {type, params, destination, queue, out = 0}).

-type sample()       :: integer() | float().
-type filter_param() :: integer() | float().
-type filter_type()  :: fir | iir.
-type filter()       :: reference() | pid().

%% api

-spec start_link(filter_type(), reference(), [filter_param()], pid()) ->
	ignore | {error, _} | {ok, pid()}.
start_link(Type, Ref, Params, Dest) ->
	gen_server:start_link({via, gproc, {n, l, Ref}}, ?MODULE, [Type, Params, Dest], []).


-spec new([filter_param()], [filter_param()], pid()) -> reference() | pid().
new(FirParams, IirParams, Destination) ->
	IirOrDest = case length(IirParams) of
		0 -> Destination;
		_ -> {iir, filter_sup:start_filter(iir, IirParams, Destination)}
	end,
	case {length(FirParams), IirOrDest} of
		{0, {iir, Ref}} -> Ref;
		{0, Destination} -> Destination;
		_ -> filter_sup:start_filter(fir, FirParams, IirOrDest)
	end.


-spec in(filter(), sample()) -> ok.
in(Filter, Value) ->
	in(Filter, Filter, Value).

-spec in(reference(), reference(), sample()) -> ok.
in(Ref, InputRef, Value) ->
	gen_server:cast({via, gproc, {n, l, Ref}}, {in, InputRef, Value}),
	ok.


-spec out(filter()) -> sample().
out(Filter) ->
	receive
		{out, Filter, Value} -> Value
	end.

%% callbacks

init([Type, Params, Dest]) ->
	Queue = lists:foldl(fun(_, Q) -> [0 | Q] end, [],
		lists:seq(1, length(Params))),
	{ok, #state{type = Type, params = Params, queue = Queue,
			destination = Dest}}.


handle_cast({in, InputRef, In}, State = #state{type = fir}) ->
	Queue = [In | State#state.queue],
	{Out, Queue1} = run(fir, Queue, State#state.params),
	case State#state.destination of
		{iir, Ref} -> in(Ref, InputRef, Out);
		Pid -> Pid ! {out, InputRef, Out}
	end,
	{noreply, State#state{queue = Queue1}};

handle_cast({in, InputRef, In}, State = #state{type = iir}) ->
	{Loop, Queue} = run(iir, State#state.queue, State#state.params), 
	Out = In + Loop,
	State#state.destination ! {out, InputRef, Out},
	Queue1 = [Out | Queue],
	{noreply, State#state{queue = Queue1, out = Out}}.


handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% internal functions

-spec run(filter_type(), [sample()], [filter_param()]) -> {sample(), [sample()]}.
run(Type, Queue, Params) ->
	run(Type, Queue, Params, [], []).

-spec run(filter_type(), [sample()], [], [sample()], [sample()]) ->
	{sample(), [sample()]}.
run(_Type, _Queue, [], Result, NewQueue) ->
	{lists:sum(Result), lists:reverse(NewQueue)};

run(fir, [QueueItem | Queue], [Param | Params], Result, NewQueue) ->
	run(fir, Queue, Params, [QueueItem * Param | Result], [QueueItem | NewQueue]);

run(iir, [QueueItem | Queue], [Param | Params], Result, NewQueue) ->
	run(iir, Queue, Params, [QueueItem * -Param | Result], [QueueItem | NewQueue]).
