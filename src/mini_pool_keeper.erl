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

-module(mini_pool_keeper).

-behaviour(gen_server).

-include("mini_pool.hrl").
-include("mini_pool_internal.hrl").

-export([ init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3]).

-record(state, { status = inactive,
                 pool_option, 
                 pids = []}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ start_link/1,
          get_status/1, 
          all_pids/1, 
          get_pid/2]).

start_link(#pool_option{name = Name} = PoolOpt) ->
    gen_server:start_link({local, Name}, ?MODULE, [PoolOpt], []).

get_status(Name) ->
    gen_server:call(Name, {get_status}).

all_pids(Name)->
    gen_server:call(Name, {all_pids}).

get_pid(Name, Id)->
    gen_server:call(Name, {get_pid, Id}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([PoolOpt]) when is_record(PoolOpt, pool_option)->
    process_flag(trap_exit, true),
    State = 
    case start_pool(PoolOpt) of 
        {ok, Pids}->
            #state{status = actived,  pool_option = PoolOpt, pids = Pids};
        _->
            #state{status = inactive, pool_option = PoolOpt}
    end,
    {ok, State}.

handle_call({all_pids}, _From, State)->
    #state{pids = Pids} = State,
    {reply, {ok, Pids}, State};
handle_call({get_pid, Id}, _From, State)->
    #state{ pids = Pids} = State,
    Reply = 
    case util_plist:get_value(Id, Pids) of
        {ok, Value} ->
            {ok, Value};
        _->
            {error, not_found}
    end,
    {reply, Reply, State};
handle_call({get_status}, _From, State)->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', DiedPid, Reason}, 
            #state{ pool_option = #pool_option{ name      = PoolName,
                                                component = Component,
                                                start_opt = StartOpt
                                                }, 
                    pids = Pids
                   } = State) ->
    
    Id       = head([Id || {Id, Pid} <- Pids, Pid =:= DiedPid]),
    NewPids  = proplists:delete(Id, Pids),
    NewState = 
    case Component:recover({Id, Reason}, StartOpt) of 
        {ok, NewPid} when is_pid(NewPid)->
            true = link(NewPid),
            State#state{pids = [{Id, NewPid}|NewPids]};
        _Other ->
            State
    end,
    true = ets:insert(?TAB_POOLS, #mini_pools{ name = PoolName, 
                                               pids = NewState#state.pids}),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{  pool_option = 
                          #pool_option{component = Component} = PoolOpt}) ->
    catch Component:on_event({keeper_terminated, Reason}, PoolOpt),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_pool(#pool_option{ name         = PoolName,
                         component    = Component,
                         start_opt    = StartOpt} = PoolOpt) ->
    case Component:start(StartOpt) of 
        {ok, Pids} ->
            [link(Pid) || {_Id, Pid} <- Pids],
            true = ets:insert(?TAB_POOLS, 
                            #mini_pools{name = PoolName, pids = Pids}),
            catch Component:on_event({pool_started, Pids}, PoolOpt),
            {ok, Pids};
        {error, Reason} ->
            {error, Reason};
        error->
            {error, unknown_error}
    end.

head([]) -> undefined;
head([H|_T]) -> H.