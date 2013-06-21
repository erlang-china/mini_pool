%% -------------------------------------------------------------------
%% Copyright (c) 2013 Xujin Zheng (zhengxujin@adsage.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% -------------------------------------------------------------------

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