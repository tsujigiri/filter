
-module(filter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_filter/3]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_filter(Type, FilterParams, Destination) ->
	Ref = make_ref(),
	{ok, _Pid} = supervisor:start_child(?MODULE, {Ref,
			{filter, start_link, [Type, Ref, FilterParams, Destination]},
			permanent, brutal_kill, worker, [filter]}),
	Ref.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

