-ifndef(MINI_POOL_H).
-define(MINI_POOL, true).

-record(pool_option, {  name           :: atom(), 
                        component      :: atom(),
                        start_opt      :: proplists:property()
                      }).

-endif.