mini_pool
======
    
You can dynamic manage your pool with mini_pool, just like supervisor, but it's different with supervisor, becasue
if the pool died, you may have different strategy to restart the connection/pid/ and so on, but supervisor only can just
restart your pool, and if you dynamic add the pool, and the supervisor died sometimes, the part of dynamic added pool 
will not restart again.

We provider the behaviour of `mini_pool_component` to help you get the pids status, and if it died, it will call 
the callback, that you could recover something.

 **Behaviour Example:**
```erlang
-module(test).
-behaviour(mini_pool_component).

-include("mini_pool.hrl").

-export([on_event/2, recover/2, start/1]).

on_event(Msg, Option) ->
    erlang:display({on_event, {Msg, Option}}).

recover({Id, Reason}, Option) ->
    erlang:display({recover, {Id, Reason}, Option}),
    NewPid = spawn(fun()-> receive {x}-> ok end end),
    {ok, NewPid}.

start(Option) ->
    erlang:display({start, Option}),
    Pid = spawn(fun()-> receive {x}-> ok end end),
    {ok, [{1, Pid}]}.
```

***supervisor's diagram***

```
                 +---------------+
                 | mini_poll_sup |
                 +-------+-------+
                         |
               +---------+--------------+
               |                        |
        +------+----+        +----------+-----------+
        | mini_pool |        | mini_pool_keeper_sup |
        +-----------+        +----------+-----------+
                                        |
                +---------------------------------------------+
                |                                             |
           +--------+                                     +--------+
           | pool_1 |                                     | pool_2 |
           +----+---+                                     +---+----+
                |                                             |
   +-------------+--------------+              +--------------+---------------+
   |             |              |              |              |               |
+----------+   +----------+   +----------+   +----------+   +----------+   +----------+
| <0.72.0> |   | <0.73.0> |   | <0.74.0> |   | <0.75.0> |   | <0.76.0> |   | <0.77.0> |
+----------+   +----------+   +----------+   +----------+   +----------+   +----------+
```
