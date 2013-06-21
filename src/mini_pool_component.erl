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