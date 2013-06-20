-type gen_fsm_state_name() :: atom().
-type gen_fsm_state_data() :: term().
-type gen_fsm_timeout() :: pos_integer() | infinity.
-type gen_fsm_stop_reason() :: term().

-type gen_fsm_start_args() :: term().

-type gen_fsm_init_result( ) ::
		{ok, gen_fsm_state_name(), gen_fsm_state_data()}
	|	{ok, gen_fsm_state_name(), gen_fsm_state_data(), gen_fsm_timeout()}
 	|	{ok, gen_fsm_state_name(), gen_fsm_state_data(), hibernate}
  	|	{stop, gen_fsm_stop_reason()}
  	|	ignore.

-spec init(gen_fsm_start_args() ) -> gen_fsm_init_result().

-type gen_fsm_event() :: term().
-type gen_fsm_handle_event_result() ::
		{next_state, gen_fsm_state_name(), gen_fsm_state_data()}
  	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data(), gen_fsm_timeout()}
 	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data(), hibernate}
 	| 	{stop, gen_fsm_stop_reason(), gen_fsm_state_data()}.

-spec handle_event(gen_fsm_event(), gen_fsm_state_name(), gen_fsm_state_data()) -> gen_fsm_handle_event_result().

-type gen_fsm_from() :: {pid(), reference()}.
-type gen_fsm_reply() :: term().
-type gen_fsm_handle_sync_event_result() ::
		{reply, gen_fsm_reply(), gen_fsm_state_name(), gen_fsm_state_data()}
  	| 	{reply, gen_fsm_reply(), gen_fsm_state_name(), gen_fsm_state_data(), gen_fsm_timeout()}
  	| 	{reply, gen_fsm_reply(), gen_fsm_state_name(), gen_fsm_state_data(), hibernate}
  	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data()}
  	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data(), gen_fsm_timeout()}
  	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data(), hibernate}
  	| 	{stop, gen_fsm_stop_reason(), gen_fsm_reply(), gen_fsm_state_data()}
  	| 	{stop, gen_fsm_stop_reason(), gen_fsm_state_data()}.


-spec handle_sync_event(gen_fsm_event(), gen_fsm_from(), gen_fsm_state_name(), gen_fsm_state_data()) -> gen_fsm_handle_sync_event_result().

-type gen_fsm_request() :: term().
-type gen_fsm_handle_info_result() ::
		{next_state, gen_fsm_state_name(), gen_fsm_state_data()}
  	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data(), gen_fsm_stop_reason()}
  	| 	{next_state, gen_fsm_state_name(), gen_fsm_state_data(), hibernate}
  	| 	{stop, gen_fsm_stop_reason(), gen_fsm_state_data()}.


-spec handle_info(gen_fsm_request(), gen_fsm_state_name(), gen_fsm_state_data()) -> gen_fsm_handle_info_result().

-spec terminate(gen_fsm_stop_reason(), gen_fsm_state_name(), gen_fsm_state_data()) -> ignored.

-type gen_fsm_extra() :: term().
-type gen_fsm_vsn() :: term() | {down, term()}.

-spec code_change(gen_fsm_vsn(), gen_fsm_state_name(), gen_fsm_state_data(), gen_fsm_extra()) -> {ok, gen_fsm_state_name(), gen_fsm_state_data()}.
