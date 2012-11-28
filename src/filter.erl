-module(filter).
-behaviour(gen_server).

%% api
-export([start/0, stop/0, new/3, start_link/3, in/2, out/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {type, params, destination, queue, out = 0}).


%% api

start() ->
	ok = application:start(filter).

stop() ->
	ok = application:stop(filter).

new(FirParams, IirParams, Destination) ->
	{ok, Sup} = filter_sup_sup:start_child(),
	IirOrDest = case length(IirParams) of
		0 -> Destination;
		_ -> filter_sup:start_child(Sup, iir, IirParams, Destination)
	end,
	Filter = case length(FirParams) of
		0 -> IirOrDest;
		_ -> filter_sup:start_child(Sup, fir, FirParams, IirOrDest)
	end,
	Filter == Destination andalso supervisor:terminate_child(filter_sup_sup, Sup),
	Filter.


start_link(Type, Params, Dest) ->
	gen_server:start_link(?MODULE, [Type, Params, Dest], []).

in(Server, Value) ->
	gen_server:cast(Server, {in, Value}),
	ok.

out(Server) ->
	receive
		{out, Server, Value} -> Value
	end.

%% callbacks

init([Type, Params, Dest]) ->
	Queue = lists:foldl(fun(_, Q) -> queue:in(0, Q) end, queue:new(),
		lists:seq(1, length(Params))),
	{ok, #state{type = Type, params = Params, queue = Queue, destination = Dest}}.


handle_cast({in, In}, State = #state{type = fir}) ->
	Out = State#state.out,
	Queue = queue:in(In, queue:drop(State#state.queue)),
	case State#state.destination of
		{iir, Pid} -> 
			Out1 = run(fir, lists:reverse(queue:to_list(Queue)), State#state.params),
			filter_iir:in(Pid, Out);
		Pid when is_pid(Pid) ->
			Pid ! {out, self(), Out},
			Out1 = run(fir, lists:reverse(queue:to_list(Queue)), State#state.params)
	end,
	{noreply, State#state{queue = Queue, out = Out1}};

handle_cast({in, In}, State = #state{type = iir}) ->
	Out = State#state.out,
	State#state.destination ! {out, self(), Out},
	Queue = State#state.queue,
	Out1 = In + run(iir, lists:reverse(queue:to_list(Queue)), State#state.params),
	Queue1 = queue:in(Out, queue:drop(Queue)),
	{noreply, State#state{queue = Queue1, out = Out1}}.



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
