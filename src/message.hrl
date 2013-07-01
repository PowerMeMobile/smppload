-ifndef(message_hrl).
-define(message_hrl, 1).

-record(address, {
	addr :: binary(),
	ton  :: pos_integer(),
	npi  :: pos_integer()
}).

-record(message, {
	source :: #address{},
	destination :: #address{},
	body :: binary(),
	esm_class = 0 :: integer(),
	delivery :: boolean()
}).

-endif.
