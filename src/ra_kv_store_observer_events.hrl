%%%-------------------------------------------------------------------
%%% @author fms
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2021 09:56
%%%-------------------------------------------------------------------
-author("fms").

-record(ra_machine_state_update_obs_event, {
  old_state :: any(),
  new_state :: any()
}).

-record(ra_machine_reply_write_obs_event, {
  index :: any(),
  term :: any()
}).

-record(ra_machine_reply_read_obs_event, {
  read :: any(),
  index :: any(),
  term :: any()
}).

-record(ra_machine_side_effects_obs_event, {
  side_effects :: any()
}).

%% copy-pasted for now
-record(obs_process_event, {
  process :: pid() | atom(),
  event_type :: any(),
  event_content :: any()
}).
