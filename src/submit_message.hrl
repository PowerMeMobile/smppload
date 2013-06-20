-ifndef(submit_message_hrl).
-define(submit_message_hrl, 1).

-record(address, {
	addr :: binary(),
	ton  :: pos_integer(),
	npie :: pos_integer()
}).

-record(submit_message, {
	source 		:: #address{},
	destination :: #address{},
	body        :: binary(),
	delivery	:: boolean()
}).

-endif.
