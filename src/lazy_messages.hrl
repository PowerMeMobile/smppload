-ifndef(lazy_messages_hrl).
-define(lazy_messages_hrl, 1).

-type key() :: atom().
-type value() :: term().
-type plist() :: [{key(), value()}].
-type config() :: plist().
-type state() :: term().
-type reason() :: term().

-endif.
