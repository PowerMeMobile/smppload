Installation
------------

In order to run the *smppload* utility you need to have [Erlang](http://www.erlang.org/), [rebar](https://github.com/basho/rebar), and make installed.

Compilation
-----------

<pre>
make escriptize
</pre>

Usage
-----

* Send a message with the body "Hello there!" to localhost and the standard SMPP port
<pre>
./smppload --source 375296660002 --destination 375293332211 --body "Hello there!"
</pre>

* The above is the same as
<pre>
./smppload --host 127.0.0.1 --port 2775 --bind-type trx --system_type "" --system_id user --password password --source 375296660002 --destination 375293332211 --body "Hello there!"
</pre>

* Send a message as TX
<pre>
./smppload --bind_type tx --source 375296660002 --destination 375293332211 --body "Hello there!"
</pre>

* Send a message with defined TON and NPI
<pre>
./smppload --source FromBank,5,0 --destination 375293332211,1,1 --body "Return our money, looser!"
</pre>

* Send a message with a random body
<pre>
./smppload --source 375296660002 --destination 375293332211
</pre>

* Send a message with a random body and length 25
<pre>
./smppload --source 375296660002 --destination 375293332211 --length 25
</pre>

* Send a multipart message with a random body and length 160
<pre>
./smppload --source 375296660002 --destination 375293332211 --length 160
</pre>

* Send 100 messages with random bodys
<pre>
./smppload --source 375296660002 --destination 375293332211 --count 100
</pre>

* Send messages from file test/messages.txt
<pre>
cat test/messages.txt
# source;destination;body;delivery
# where
#   source :: address
#   destination :: address
#   address :: addr[,ton,npi]
#   body :: string, to use double semicolon (;;) in the body
#   delivery :: true | false | 1 | 0
375296660002,1,1;375291112231,1,1;Message #1;true
375296660002,1,1;375291112232,1,1;Message #2;true
375296660002,1,1;375291112233,1,1;Message #3;true
375296660002,1,1;375291112234,1,1;Message #4;true
375296660002,1,1;375291112235,1,1;Message #5;true
./smppload --file test/messages.txt
</pre>

* Send messages from standard input
<pre>
cat test/messages.txt | ./smppload --file -
</pre>

* Send messages dynamically generated messages from standard input
<pre>
for i in `seq 1 100`; do
	printf "375296660002,1,1;37529%07d,1,1;Message #%d;false\n" $i $i
done | ./smppload --file -
</pre>

* Send a message with ERROR (default) log level
<pre>
./smppload --source 375296660002 --destination 375293332211 --body "Hello there!"
</pre>

* Send a message with INFO log level
<pre>
./smppload --source 375296660002 --destination 375293332211 --body "Hello there!" -v
INFO:  Connected to 127.0.0.1:2775
INFO:  Bound to Funnel
INFO:  Stats:
INFO:     Send success:     1
INFO:     Delivery success: 0
INFO:     Send fail:        0
INFO:     Delivery fail:    0
INFO:     Errors:           0
INFO:     Avg Rps:          20 mps
INFO:  Unbound
</pre>

* Send a message with DEBUG log level
<pre>
./smppload --source 375296660002 --destination 375293332211 --body "Hello there!" -vv
DEBUG: Options: [{port,2775},
                 {system_type,[]},
                 {system_id,"user"},
                 {password,"password"},
                 {source,"375296660002"},
                 {destination,"375293332211"},
                 {body,"Hello there"},
                 {verbosity,2},
                 {host,"127.0.0.1"},
                 {rps,1000},
                 {length,140},
                 {count,1},
                 {delivery,0}]
DEBUG: Module: lazy_messages_body
INFO:  Connected to 127.0.0.1:2775
DEBUG: Request: {bind_transceiver,[{system_type,[]},
                                   {system_id,"user"},
                                   {password,"password"}]}
DEBUG: Response: {bind_transceiver_resp,0,1,[{system_id,"Funnel"}]}
INFO:  Bound to Funnel
DEBUG: Request: {submit_sm,[{source_addr_ton,1},
                            {source_addr_npi,1},
                            {source_addr,"375296660002"},
                            {dest_addr_ton,1},
                            {dest_addr_npi,1},
                            {destination_addr,"375293332211"},
                            {short_message,"Hello there"},
                            {registered_delivery,0}]}
DEBUG: Response: {submit_sm_resp,0,2,[{message_id,"190602"}]}
INFO:  Stats:
INFO:     Send success:     1
INFO:     Delivery success: 0
INFO:     Send fail:        0
INFO:     Delivery fail:    0
INFO:     Errors:           0
INFO:     Avg Rps:          23 mps
DEBUG: Request: {unbind,[]}
DEBUG: Response: {unbind_resp,0,3,[]}
INFO:  Unbound
</pre>

Known issues and limitations
----------------------------

* Default encoding is Latin 1. Max message length is 140 chars, max segment length is 134 chars
* Don't send multiple messages to the same destination number in the same session.
