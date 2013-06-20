-ifndef(MINI_POOL_INTERNAL_H).
-define(MINI_POOL_INTERNAL_H, true).

-define(TAB_POOL_CONFIGS, ets_mini_pool_configs).
-define(TAB_POOLS,        ets_mini_pool_pids).
-define(POOL_KEEPER_SUP,  mini_pool_keeper_sup).
-define(POOL_KEEPER,      mini_pool_keeper).


-record(mini_pools, {name, pids = orddict:new()}).

-endif.