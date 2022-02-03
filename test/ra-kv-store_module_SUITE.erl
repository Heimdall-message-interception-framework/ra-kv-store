-module('ra-kv-store_module_SUITE').
-include("test_engine_types.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, test_engine/1, test_module/1, init_per_testcase/2, init_per_suite/1, end_per_suite/1, end_per_testcase/2, test_bfs_scheduler/1, test_pct_scheduler/1, test_random_scheduler/1, test_fifo_scheduler/1]).

all() -> [
   test_module,
   test_engine,
   test_bfs_scheduler,
   test_pct_scheduler
].

init_per_suite(Config) ->
  application:load(ra),
%%  MIL
  logger:set_primary_config(level, warning),
  LogConf = #{config => #{file => "./MIL_log.log"}, level => warning},
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
%% OBS
%%  {ok, _} = gen_event:start(om),
%%  ok = gen_event:add_handler(om, raft_election_safety, []),
%%  ok = gen_event:add_handler(om, raft_leader_append_only, []),
%%  ok = gen_event:add_handler(om, raft_leader_completeness, []),
%%  ok = gen_event:add_handler(om, raft_state_machine_safety, []),
%%  ok = gen_event:add_handler(om, raft_log_matching, []),
%% SBO
  Config ++ [{html_output, true}, {test_module, ?MODULE}, {test_name, TestCase}].

end_per_testcase(_, Config) ->
  Config.

test_module(InitialConfig) ->
  % MIL: start OM, message interception_layer and (initial) scheduler
  {ok, OM} = gen_event:start({local,om}),
  {ok, MIL} = message_interception_layer:start(),
  erlang:monitor(process, MIL),

  Conf = maps:from_list([{num_processes, 3}] ++ InitialConfig),
  {ok, _RKVMod} = 'ra-kv-store_module':start_link(Conf),
  'ra-kv-store_module':bootstrap_wo_scheduler(),

%%  AbstrInst = #abstract_instruction{module = ra_kv_store, function = read},
%%  erlang:display('ra-kv-store_module':generate_instruction(AbstrInst)),
%%  io:format("~p~n", ['ra-kv-store_module':generate_instruction(AbstrInst)]),
%%  io:format("~p~n", ['ra-kv-store_module':generate_instruction(AbstrInst)]),
%%  io:format("~p~n", ['ra-kv-store_module':generate_instruction(AbstrInst)]),
%%  io:format("~p~n", ['ra-kv-store_module':generate_instruction(AbstrInst)]),

  % check if our monitors report anything
  receive Msg ->
    io:format("Received: ~p~n", [Msg])
  after 0 ->
    ok
  end,

  % kill other processes
  gen_server:stop(MIL),
%%  gen_server:stop(Scheduler),
%%  gen_server:stop(CTH),
  gen_event:stop(OM).


test_engine(InitialConfig) ->
  {ok, Engine} = gen_server:start_link(test_engine, ['ra-kv-store_module', scheduler_random], []),
  MILInstructions = [],
  Conf = maps:from_list([{num_processes, 2}] ++ InitialConfig),
  Timeout = infinity,
  Runs = test_engine:explore(Engine, 'ra-kv-store_module', Conf,
                             MILInstructions, 1, 40, Timeout), % 100, 5
  lists:foreach(fun({RunId, History}) -> io:format("Run ~p: ~p", [RunId,History]) end, Runs).


test_bfs_scheduler(InitialConfig) ->
  Conf = InitialConfig ++ [{scheduler, scheduler_bfs}],
  test_scheduler_general(Conf).

test_pct_scheduler(InitialConfig) ->
  Conf = InitialConfig ++ [{scheduler, scheduler_pct}],
  test_scheduler_general(Conf).

test_random_scheduler(InitialConfig) ->
  Conf = InitialConfig ++ [{scheduler, scheduler_random_capped}],
  test_scheduler_general(Conf).

test_fifo_scheduler(InitialConfig) ->
  Conf = InitialConfig ++ [{scheduler, scheduler_fifo_capped}],
  test_scheduler_general(Conf).

test_scheduler_general(InitialConfig) ->
  Conf = maps:from_list([
    {num_processes, list_to_integer(os:getenv("NUM_PROCESSES", "2"))},
    {num_possible_dev_points, 40},
    {num_runs, list_to_integer(os:getenv("NUM_RUNS", "2"))},
    {run_length, list_to_integer(os:getenv("RUN_LENGTH", "40"))},
    {size_d_tuple, list_to_integer(os:getenv("SIZE_D", "5"))}
  ] ++ InitialConfig),
  {ok, Engine} = gen_server:start_link(test_engine, ['ra-kv-store_module', maps:get(scheduler, Conf), Conf], []),
  MILInstructions = [],
  Timeout = infinity,
  Runs = test_engine:explore(Engine, 'ra-kv-store_module', Conf,
    MILInstructions, maps:get(num_runs, Conf), maps:get(run_length, Conf), Timeout),
  gen_server:stop(Engine),
  lists:foreach(fun({RunId, History}) -> io:format("Run ~p: ~p", [RunId,History]) end, Runs).