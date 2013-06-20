-module(mini_pool_keeper_sup).

-behaviour(supervisor).

-include("mini_pool.hrl").
-include("mini_pool_internal.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Pools    = get_pools_from_ets(),
    Children = 
    [pool_to_child_spec(Pool) ||Pool<- Pools],
    {ok, { {one_for_one, 50, 100}, Children}}.

get_pools_from_ets()->
    ets:match_object(?TAB_POOL_CONFIGS, '$1').

pool_to_child_spec(PoolOpt) when is_record(PoolOpt, pool_option)->
    #pool_option{name = PoolName} = PoolOpt,
    {PoolName, 
            {?POOL_KEEPER,  start_link,  [PoolOpt]},
             permanent, 
             5000, 
             worker, 
             [?POOL_KEEPER]}.