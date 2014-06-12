-ifndef(message_hrl).
-define(message_hrl, 1).

-record(rand_addr, {
    prefix :: string(),
    rand_len :: pos_integer()
}).

-record(address, {
    addr :: string() | #rand_addr{},
    ton :: pos_integer(),
    npi :: pos_integer()
}).

-record(message, {
    source :: #address{},
    destination :: #address{},
    body :: binary(),
    esm_class = 0 :: integer(),
    data_coding :: integer(),
    delivery :: boolean()
}).

-endif.
