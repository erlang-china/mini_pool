-module(mini_pool_component).

-include("mini_pool.hrl").

-callback on_event( Msg       :: tuple(), 
                   PoolOption :: #pool_option{}) -> ok.

-callback recover( {Id :: integer(), Reason :: term()}, 
                   StartOption :: term()) ->
    {ok, Pid :: pid()} 
    | ignore
    | {error, Reason :: term()}.

-callback start(StartOption :: term()) ->
    {ok, [pid()]} 
    | {error, Reason :: term()}.