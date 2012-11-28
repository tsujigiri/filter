
-module(filter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/4]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

start_child(Sup, Type, FilterParams, Destination) ->
	{ok, Pid} = supervisor:start_child(Sup, {make_ref(),
			{filter, start_link, [Type, FilterParams, Destination]},
			permanent, brutal_kill, worker, [filter_fir]}),
	Pid.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_all, 5, 10}, []}}.

