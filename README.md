mini_pool
======

example:

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