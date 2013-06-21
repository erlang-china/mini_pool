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

-ifndef(MINI_POOL_INTERNAL_H).
-define(MINI_POOL_INTERNAL_H, true).

-define(TAB_POOL_CONFIGS, ets_mini_pool_configs).
-define(TAB_POOLS,        ets_mini_pool_pids).
-define(POOL_KEEPER_SUP,  mini_pool_keeper_sup).
-define(POOL_KEEPER,      mini_pool_keeper).


-record(mini_pools, {name, pids = orddict:new()}).

-endif.