-module(filter).
-behaviour(gen_server).

%% api
-export([start/0, stop/0, new/3, start_link/4, in/2, out/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {type, params, destination, queue, out = 0}).

%% api

start() -> ok = application:start(filter).

stop() -> ok = application:stop(filter).


start_link(Type, Ref, Params, Dest) ->
	gen_server:start_link({via, gproc, {n, l, Ref}}, ?MODULE, [Type, Params, Dest], []).


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


in(Filter, Value) ->
	in(Filter, Filter, Value).

in(Ref, InputRef, Value) ->
	gen_server:cast({via, gproc, {n, l, Ref}}, {in, InputRef, Value}),
	ok.


out(Filter) ->
	receive
		{out, Filter, Value} -> Value
	end.

%% callbacks

init([Type, Params, Dest]) ->
	Queue = lists:foldl(fun(_, Q) -> queue:in(0, Q) end, queue:new(),
		lists:seq(1, length(Params))),
	{ok, #state{type = Type, params = Params, queue = Queue,
			destination = Dest}}.


handle_cast({in, InputRef, In}, State = #state{type = fir}) ->
	Out = State#state.out,
	Queue = queue:in(In, queue:drop(State#state.queue)),
	case State#state.destination of
		{iir, Ref} ->
			Out1 = run(fir, lists:reverse(queue:to_list(Queue)), State#state.params),
			in(Ref, InputRef, Out1);
		Pid ->
			Pid ! {out, InputRef, Out},
			Out1 = run(fir, lists:reverse(queue:to_list(Queue)), State#state.params)
	end,
	{noreply, State#state{queue = Queue, out = Out1}};

handle_cast({in, InputRef, In}, State = #state{type = iir}) ->
	Out = State#state.out,
	State#state.destination ! {out, InputRef, Out},
	Queue = queue:in(Out, queue:drop(State#state.queue)),
	Out1 = In + run(iir, lists:reverse(queue:to_list(Queue)), State#state.params),
	{noreply, State#state{queue = Queue, out = Out1}}.



handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% internal functions

run(Type, Queue, Params) ->
	run(Type, Queue, Params, []).

run(_Type, [], [], Result) ->
	lists:sum(Result);

run(fir, [QueueItem | Queue], [Param | Params], Result) ->
	run(fir, Queue, Params, [QueueItem * Param | Result]);

run(iir, [QueueItem | Queue], [Param | Params], Result) ->
	run(iir, Queue, Params, [QueueItem * -Param | Result]).
