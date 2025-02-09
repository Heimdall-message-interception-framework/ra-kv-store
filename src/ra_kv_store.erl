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

-module(ra_kv_store).
-behaviour(ra_machine).

%% OBS
-include("ra_kv_store_observer_events.hrl").
%% SBO

-export([init/1,
         apply/3,
         write/3,
         read/2,
         cas/4]).

write(ServerReference, Key, Value) ->
%%    erlang:display(["ra_kv_store:write:26", "self", self(), "ServerReference", ServerReference, "Key", Key, "Value", Value]),
    Cmd = {write, Key, Value},
    case ra:process_command(ServerReference, Cmd) of
        {ok, {Index, Term}, LeaderRaNodeId} ->
            {ok, {{index, Index}, {term, Term}, {leader, LeaderRaNodeId}}};
        {timeout, _} -> timeout
    end.

read(ServerReference, Key) ->
%%    erlang:display(["ra_kv_store:read:35", "self", self(), "ServerReference", ServerReference, "Key", Key]),
    case ra:consistent_query(ServerReference,
                                         fun(State) ->
                                                 maps:get(Key, State, undefined)
                                         end) of
        {ok, Value, _} ->
            Value;
        {timeout, _} ->
            timeout;
        {error,nodedown} ->
            error
    end.

cas(ServerReference, Key, ExpectedValue, NewValue) ->
%%    erlang:display(["ra_kv_store:cas:49", "self", self(), "ServerReference", ServerReference, "Key", Key, "Exp", ExpectedValue, "New", NewValue]),
    Cmd = {cas, Key, ExpectedValue, NewValue},
    case ra:process_command(ServerReference, Cmd) of
        {ok, {{read, ReadValue}, {index, Index}, {term, Term}}, LeaderRaNodeId} ->
            {ok, {{read, ReadValue},
                  {index, Index},
                  {term, Term},
                  {leader, LeaderRaNodeId}}};
        {timeout, _} -> timeout
    end.

init(_Config) ->
  #{}.

apply(#{index := Index,
        term := Term} = _Metadata, {write, Key, Value}, State) ->
    NewState = maps:put(Key, Value, State),
    SideEffects = side_effects(Index, NewState),
%%  OBS
    RaMachineStateUpdateEvent = #ra_machine_state_update_obs_event{old_state=State, new_state=NewState},
    gen_event:sync_notify(om,
      {process, #obs_process_event{process=self(), event_type=ra_machine_state_update, event_content=RaMachineStateUpdateEvent}}),
    RaMachineReplyWriteEvent = #ra_machine_reply_read_obs_event{index=Index, term=Term},
    gen_event:sync_notify(om,
      {process, #obs_process_event{process=self(), event_type=ra_machine_reply_read, event_content=RaMachineReplyWriteEvent}}),
    RaMachineSideEffects = #ra_machine_side_effects_obs_event{side_effects=SideEffects},
    gen_event:sync_notify(om,
      {process, #obs_process_event{process=self(), event_type=ra_machine_side_effects, event_content=RaMachineSideEffects}}),
%%  SBO
    %% return the index and term here as a result
    {NewState, {Index, Term}, SideEffects};
apply(#{index := Index, term := Term} = _Metadata,
      {cas, Key, ExpectedValue, NewValue}, State) ->
    {NewState, ReadValue} = case maps:get(Key, State, undefined) of
                                ExpectedValue ->
                                    {maps:put(Key, NewValue, State), ExpectedValue};
                                ValueInStore ->
                                    {State, ValueInStore}
                            end,
    SideEffects = side_effects(Index, NewState),
%%  OBS
    RaMachineStateUpdateEvent = #ra_machine_state_update_obs_event{old_state=State, new_state=NewState},
    gen_event:sync_notify(om,
      {process, #obs_process_event{process=self(), event_type=ra_machine_state_update, event_content=RaMachineStateUpdateEvent}}),
    RaMachineReplyReadEvent = #ra_machine_reply_read_obs_event{read=ReadValue, index=Index, term=Term},
    gen_event:sync_notify(om,
      {process, #obs_process_event{process=self(), event_type=ra_machine_reply_read, event_content= RaMachineReplyReadEvent}}),
    RaMachineSideEffects = #ra_machine_side_effects_obs_event{side_effects=SideEffects},
    gen_event:sync_notify(om,
      {process, #obs_process_event{process=self(), event_type=ra_machine_side_effects, event_content= RaMachineSideEffects}}),
%%  SBO
    {NewState, {{read, ReadValue}, {index, Index}, {term, Term}}, SideEffects}.

side_effects(RaftIndex, MachineState) ->
    case application:get_env(ra_kv_store, release_cursor_every) of
        undefined ->
            [];
        {ok, NegativeOrZero} when NegativeOrZero =< 0 ->
            [];
        {ok, Every} ->
            case release_cursor(RaftIndex, Every) of
                release_cursor ->
                    [{release_cursor, RaftIndex, MachineState}];
                _ ->
                    []
            end
    end.

release_cursor(Index, Every) ->
    case Index rem Every of
        0 ->
            release_cursor;
        _ ->
            do_not_release_cursor
    end.

