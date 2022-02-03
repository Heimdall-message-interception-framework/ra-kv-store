%% Copyright (c) 2018 Pivotal Software Inc, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%       https://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(store_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [
      kv_store
    ].

group() -> [].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    application:load(ra),
%%  MIL
    logger:set_primary_config(level, debug),
    LogConf = #{config => #{file => "./MIL_log.log"}, level => debug},
    logger:add_handler(myhandler, logger_std_h, LogConf),
%%  LIM
    WorkDirectory = proplists:get_value(priv_dir, Config),
    ok = application:set_env(ra, data_dir, filename:join(WorkDirectory, "ra")),
    ok = application:set_env(ra_kv_store, release_cursor_every, 1),
    Config.

end_per_suite(Config) ->
    application:stop(ra),
    Config.

init_per_testcase(TestCase, Config) ->
%% MIL
    {_, ConfigReadable} = logging_configs:get_config_for_readable(TestCase),
    logger:add_handler(readable_handler, logger_std_h, ConfigReadable),
    {_, ConfigMachine} = logging_configs:get_config_for_machine(TestCase),
    logger:add_handler(machine_handler, logger_std_h, ConfigMachine),
    % MIL: start scheduler and message interception_layer
    {ok, _MIL} = message_interception_layer:start(),
    {ok, Scheduler} = scheduler_naive:start(),
    {ok, CTH} = commands_transfer_helper:start_link(Scheduler),
    commands_transfer_helper:start(CTH),
%% LIM
%% OBS
    {ok, _} = gen_event:start({local, om}),
    ok = gen_event:add_handler(om, raft_election_safety, []),
    ok = gen_event:add_handler(om, raft_leader_append_only, []),
    ok = gen_event:add_handler(om, raft_leader_completeness, []),
    ok = gen_event:add_handler(om, raft_state_machine_safety, []),
    ok = gen_event:add_handler(om, raft_log_matching, []),
%% SBO
    Config.

end_per_testcase(_TestCase, _Config) ->
    gen_server:stop(commands_transfer_helper),
    gen_server:stop(scheduler_naive),
    gen_server:stop(message_interception_layer),
    ok.

%% -------------------------------------------------------------------
%% Testcases.
%% -------------------------------------------------------------------

kv_store(_Config) ->
    Nodes = [{ra_kv1, node()}, {ra_kv2, node()}, {ra_kv3, node()}],
    ClusterId = <<"ra_kv_store">>,
    Config = #{},
    Machine = {module, ra_kv_store, Config},
    application:ensure_all_started(ra),

    ok = ra:start(), % did not work w/o MIL in parameters
%%    ra_system:start_default(),

    {ok, _, _} = ra:start_cluster(default, ClusterId, Machine, Nodes),
    {ok, _} = ra_kv_store:write(ra_kv1, 1, 2),
    2 = ra_kv_store:read(ra_kv1, 1),
    {ok, {{read, 2}, _, _, _}} = ra_kv_store:cas(ra_kv1, 1, 2, 4),
    {ok, {{read, 4}, _, _, _}} = ra_kv_store:cas(ra_kv1, 1, 3, 6),
    4 = ra_kv_store:read(ra_kv1, 1),

    {ok, {{read, undefined}, _, _, _}} = ra_kv_store:cas(ra_kv1, 2, undefined, 1),
    1 = ra_kv_store:read(ra_kv1, 2),
    {ok, {{read, 1}, _, _, _}} = ra_kv_store:cas(ra_kv1, 2, undefined, 3),
    1 = ra_kv_store:read(ra_kv1, 2),
    ok.