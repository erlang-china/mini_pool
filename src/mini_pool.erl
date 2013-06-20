-module(mini_pool).
-behaviour(gen_server).

-include("mini_pool.hrl").
-include("mini_pool_internal.hrl").

-export([start_link/0]).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-export([start/0, stop/0]).

-export([start_pool/1, get_pool/1, get_pool/2, is_pool_exists/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_pool(PoolOpt) when is_record(PoolOpt, pool_option) ->
    gen_server:call(?SERVER, {start_pool, PoolOpt}).

get_pool(PoolName) ->
    case ets:lookup(?TAB_POOLS, PoolName) of 
        [#mini_pools{pids = Pids}] ->
            {ok, Pids};
        _->
            {error, pool_not_found}
    end.
    
get_pool(PoolName, Id) ->
    case get_pool(PoolName) of 
        {ok, Pids} ->
            case util_plist:get_value(Id, Pids) of 
                Pid when is_pid(Pid)->
                    {ok, Pid};
                undefined ->
                    {error, pid_not_found}
            end;
        Error ->
            Error
    end.

start()->
    ensure_started(ets_mgr),
    application:start(?MODULE).

stop()->
    application:stop(?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ok = init_ets(?TAB_POOL_CONFIGS, protected),
    ok = init_ets(?TAB_POOLS, public),
    {ok, #state{}}.

handle_call({start_pool, PoolOpt}, _From, State) ->
    Reply = internal_start_pool(PoolOpt),
    {reply, Reply, State};
handle_call({start_stop, PoolName}, _From, State) ->
    Reply = internal_stop_pool(PoolName),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

init_ets(Tab, Type)->
    Tab = ets_mgr:soft_new( Tab, 
                            [named_table,
                             Type,
                             {keypos, #pool_option.name},
                             {write_concurrency, false}, 
                             {read_concurrency,  true}]),
    ok.


internal_start_pool(PoolOpt) when is_record(PoolOpt, pool_option) ->
    #pool_option{ name      = PoolName,
                  component = Module} = PoolOpt,
    case check_callback_component(Module) of 
        ok -> 
            case whereis(?POOL_KEEPER_SUP) of
                SupPid when is_pid(SupPid)->  
                    case supervisor:start_child(?POOL_KEEPER_SUP,
                                                {PoolName, {?POOL_KEEPER, 
                                                             start_link, 
                                                             [PoolOpt]},
                                                permanent, 
                                                5000, 
                                                worker, 
                                                [?POOL_KEEPER]}) of 
                        {ok, Pid} ->
                            true = ets:insert(?TAB_POOL_CONFIGS, PoolOpt),
                            {ok, Pid};
                        {error, {already_started, Pid}} ->
                            {ok, Pid};
                        Error->
                            Error
                    end;
                _->
                    {error, keeper_sup_not_started}
            end;
        Error ->
            Error
    end.

internal_stop_pool(PoolName)->
    case is_pool_exists(PoolName) of
        true->
            supervisor:terminate_child(?POOL_KEEPER_SUP, PoolName),
            supervisor:delete_child(?POOL_KEEPER_SUP, PoolName);
        false->
            ok
    end.

is_pool_exists(PoolName) ->
    MatchedSups =
    [{Id, Child, Type, Modules} 
     ||{Id, Child, Type, Modules} 
        <- get_sup_children(?POOL_KEEPER_SUP), 
        Id =:= PoolName],
    length(MatchedSups) > 0.

get_sup_children(SupName) ->
    Sups0 = (catch supervisor:which_children(SupName)),
    case Sups0 of 
        {'EXIT', _} ->
            [];
        Sups ->
            Sups
    end.

check_callback_component(Module) ->
    Behaviours    = mini_pool_component:behaviour_info(callbacks),
    ModuleExports = Module:module_info(exports),
    UnExported    = Behaviours -- ModuleExports,
    case UnExported of 
        [] ->
            ok;
        UnE ->
            {error,{function_unexported, UnE}}
    end.

