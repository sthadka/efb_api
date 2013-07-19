-module(efb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(POOL_SIZE, 5).
-define(POOL_OVERFLOW_SIZE, 5).

% -------------------------------------------------------------------
% API functions
% -------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% -------------------------------------------------------------------
% Supervisor callbacks
% -------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_one, 5, 10}, [graph_pool()]} }.


% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------

graph_pool() ->
    Name = fb_graph_pool,
    WorkerArgs = [],
    PoolArgs = [{name, {local, Name}}, {worker_module, efb_graph_worker},
                {size, graph_pool_size()},
                {max_overflow, graph_pool_overflow_size()}],
    poolboy:child_spec(Name, PoolArgs, WorkerArgs).

graph_pool_size() ->
    case catch efb_conf:get(pool_size) of
        {'EXIT', _} -> ?POOL_SIZE;
        undefined   -> ?POOL_SIZE;
        Size        -> Size
    end.

graph_pool_overflow_size() ->
    case catch efb_conf:get(pool_overflow_size) of
        {'EXIT', _} -> ?POOL_OVERFLOW_SIZE;
        undefined   -> ?POOL_OVERFLOW_SIZE;
        Size        -> Size
    end.
