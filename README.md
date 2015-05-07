[![Build Status](https://travis-ci.org/PowerMeMobile/smppload.png?branch=master)](https://travis-ci.org/PowerMeMobile/smppload)

## Prerequisites

In order to compile **smppload** you need to have [Erlang](http://www.erlang.org/) and [GNU Make](http://www.gnu.org/software/make/) installed.

## Compilation

<pre>
$ git clone https://github.com/PowerMeMobile/smppload.git
$ cd smppload
$ make
</pre>

## Usage

Now it's possible to launch **smppload** as an escript, which is faster, but Erlang needs to be installed:

<pre>
$ ./smppload
</pre>

or as a release, which is slower, but has greater portability:

<pre>
$ ./rel/smppload/smppload
</pre>

* Help message

<pre>
$ ./smppload
SMPP Loader from Power Alley Gateway Suite (2.2.0)
Usage: /home/ten0s/bin/smppload [-h] [-H [&lt;host&gt;]] [-P [&lt;port&gt;]]
                                [-B [&lt;bind_type&gt;]] [-i [&lt;system_id&gt;]]
                                [-p [&lt;password&gt;]] [-t [&lt;system_type&gt;]]
                                [-r [&lt;rps&gt;]] [-s [&lt;source&gt;]]
                                [-d &lt;destination&gt;] [-b &lt;body&gt;]
                                [-l [&lt;length&gt;]] [-c [&lt;count&gt;]]
                                [-D [&lt;delivery&gt;]] [-C [&lt;data_coding&gt;]]
                                [-f &lt;file&gt;] [-v [&lt;verbosity&gt;]]
                                [-T [&lt;thread_count&gt;]]
                                [--bind_timeout [&lt;bind_timeout&gt;]]
                                [--unbind_timeout [&lt;unbind_timeout&gt;]]
                                [--submit_timeout [&lt;submit_timeout&gt;]]
                                [--delivery_timeout [&lt;delivery_timeout&gt;]]

  -h, --help          Show this message
  -H, --host          SMSC server host name or IP address [default:
                      127.0.0.1]
  -P, --port          SMSC server port [default: 2775]
  -B, --bind_type     SMSC bind type: TX | TRX | RX [default: TRX]
  -i, --system_id     SMSC system_id [default: user]
  -p, --password      SMSC password [default: password]
  -t, --system_type   SMSC service_type [default: ]
  -r, --rps           Number of requests per second. Ignored for RX
                      [default: 1000]
  -s, --source        SMS source address Addr[:Len=0][,Ton=1,Npi=1].
                      Ignored for RX [default: ]
  -d, --destination   SMS destination address Addr[:Len=0][,Ton=1,Npi=1].
                      Ignored for RX
  -b, --body          SMS body, randomly generated if not set. Ignored for
                      RX
  -l, --length        Randomly generated body length. Ignored for RX
                      [default: 140]
  -c, --count         Count of SMS to send. Ignored for RX [default: 1]
  -D, --delivery      Delivery receipt. Ignored for RX [default: 0]
  -C, --data_coding   Data coding. Ignored for RX [default: 3]
  -f, --file          Send messages from file. Ignored for RX
  -v, --verbosity     Verbosity level [default: 1]
  -T, --thread_count  Thread/process count. Ignored for RX [default: 10]
  --bind_timeout      Bind timeout, ms [default: 10000]
  --unbind_timeout    Unbind timeout, ms [default: 5000]
  --submit_timeout    Submit timeout, ms. Ignored for RX [default: 20000]
  --delivery_timeout  Delivery timeout, ms [default: TX=80000, RX=infinity]
</pre>

* Bind only
<pre>
$ ./smppload --host 127.0.0.1 --port 2775 --system_type '' --system_id user --password password --count 0
OR short
$ ./smppload -H 127.0.0.1 -P 2775 -i user -p password -c 0
</pre>

* Send a message with the body 'Hello there!' to localhost and the standard SMPP port
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --body 'Hello there!'
OR short
$ ./smppload -s 375296660002 -d 375293332211 -b 'Hello there!'
</pre>

* The above is the same as
<pre>
$ ./smppload --host 127.0.0.1 --port 2775 --bind_type trx --system_type '' --system_id user --password password --source 375296660002 --destination 375293332211 --body 'Hello there!'
OR short
$ ./smppload -H 127.0.0.1 -P 2775 -B trx -t '' -i user -p password -s 375296660002 -d 375293332211 -b 'Hello there!'
</pre>

* Send a message as TX
<pre>
$ ./smppload --bind_type tx --source 375296660002 --destination 375293332211 --body 'Hello there!'
</pre>

* Send a message with defined TON and NPI
<pre>
$ ./smppload --source FromBank,5,0 --destination 375293332211,1,1 --body 'Return our money, looser!'
</pre>

* Send a message with random trailing 4 and 7 digits respectively
<pre>
$ ./smppload --source 37529000:4 --destination 37529:7 --body 'Hi!'
</pre>

* Send a message with an empty source
<pre>
$ ./smppload --source "" --destination 375293332211 --body 'Hi!'
OR
$ ./smppload --destination 375293332211 --body 'Hi!'
</pre>

* Send a message with an empty source, but TON and NPI defined
<pre>
$ ./smppload --source ",5,0" --destination 375293332211 --body 'Hi!'
</pre>

* Send a message with a random body
<pre>
$ ./smppload --source 375296660002 --destination 375293332211
</pre>

* Send a message with a random body and length 25
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --length 25
</pre>

* Send a multipart message with a random body and length 160
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --length 160
</pre>

* Send 100 messages with random bodies
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --count 100
</pre>

* Send a message in data_coding 8 (UCS2-BE)
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --body "Привет" --data_coding 8
</pre>

* Send messages from file test/messages.txt
<pre>
$ cat test/messages.txt
# source;destination;body;delivery;data_coding
# where
#   source      :: address
#   destination :: address
#   address     :: addr[,ton,npi]
#   body        :: string, use double semicolon (;;) in the body
#   delivery    :: true | false | 1 | 0
#   data_coding :: integer
375296660002,1,1;375291112231,1,1;Message #1;true;3
375296660002,1,1;375291112232,1,1;Message #2;true;3
375296660002,1,1;375291112233,1,1;Message #3;true;3
375296660002,1,1;375291112234,1,1;Message #4;true;3
375296660002,1,1;375291112235,1,1;Message #5;true;3
$ ./smppload --file test/messages.txt
</pre>

* Send messages from standard input
<pre>
$ cat test/messages.txt | ./smppload --file -
</pre>

* Send dynamically generated messages from standard input
<pre>
$ for i in `seq 1 100`; do printf "375296660002,1,1;37529%07d,1,1;Message #%d;false;3\n" $i $i; done | ./smppload --file -
</pre>

* Send a message with ERROR log level
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --body 'Hello there!' -v0
</pre>

* Send a message with INFO (default) log level
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --body 'Hello there!' -v
INFO:  Connected to 127.0.0.1:2775
INFO:  Bound to Funnel
INFO:  Stats:
INFO:     Send success:     1
INFO:     Send fail:        0
INFO:     Delivery success: 0
INFO:     Delivery fail:    0
INFO:     Incomings:        0
INFO:     Errors:           0
INFO:     Avg Rps:          20 mps
INFO:  Unbound
</pre>

* Send a message with DEBUG log level
<pre>
$ ./smppload --source 375296660002 --destination 375293332211 --body 'Hello there!' -vv
DEBUG: Options: [{source,"375296660002"},
                 {destination,"375293332211"},
                 {body,"Hello there!"},
                 {verbosity,2},
                 {host,"127.0.0.1"},
                 {port,2775},
                 {bind_type,"trx"},
                 {system_id,"user"},
                 {password,"password"},
                 {system_type,[]},
                 {rps,1000},
                 {length,140},
                 {count,1},
                 {delivery,0},
                 {data_coding,3},
                 {thread_count,10},
                 {bind_timeout,10000},
                 {unbind_timeout,5000},
                 {submit_timeout,20000},
                 {delivery_timeout,80000}]
DEBUG: Module: lazy_messages_body
INFO:  Connected to 127.0.0.1:2775
DEBUG: Request: {bind_transceiver,[{system_type,[]},
                                   {system_id,"user"},
                                   {password,"password"},
                                   {bind_timeout,10000}]}
DEBUG: Response: {bind_transceiver_resp,0,1,[{system_id,"Funnel"}]}
INFO:  Bound to Funnel
DEBUG: Request: {submit_sm,[{source_addr_ton,1},
                            {source_addr_npi,1},
                            {source_addr,"375296660002"},
                            {dest_addr_ton,1},
                            {dest_addr_npi,1},
                            {destination_addr,"375293332211"},
                            {short_message,"Hello there"},
                            {esm_class, 0},
                            {data_coding,3},
                            {registered_delivery,0},
                            {submit_timeout,20000},
                            {delivery_timeout,80000}]}
DEBUG: Response: {submit_sm_resp,0,2,[{message_id,"190602"}]}
INFO:  Stats:
INFO:     Send success:     1
INFO:     Send fail:        0
INFO:     Delivery success: 0
INFO:     Delivery fail:    0
INFO:     Incomings:        0
INFO:     Errors:           0
INFO:     Avg Rps:          23 mps
DEBUG: Request: {unbind,[]}
DEBUG: Response: {unbind_resp,0,3,[]}
INFO:  Unbound
</pre>

* Bind as RX and keep an active connection. Stop with Ctrl-C.
<pre>
$ ./smppload --bind_type rx -vv
DEBUG: Options: [{bind_type,"rx"},
                 {verbosity,2},
                 {host,"127.0.0.1"},
                 {port,2775},
                 {system_id,"user"},
                 {password,"password"},
                 {system_type,[]},
                 {rps,1000},
                 {source,''},
                 {length,140},
                 {count,1},
                 {delivery,0},
                 {data_coding,3},
                 {thread_count,10},
                 {bind_timeout,10000},
                 {unbind_timeout,5000},
                 {submit_timeout,20000}]
INFO:  Connected to 127.0.0.1:2775
DEBUG: BindTypeFun: bind_receiver
DEBUG: Request: {bind_receiver,[{system_type,[]},
                                {system_id,"user"},
                                {password,"password"},
                                {bind_timeout,10000}]}
DEBUG: Response: {bind_receiver_resp,0,1,[{system_id,"Funnel"}]}
INFO:  Bound to Funnel
^C
</pre>

* Bind as RX with 5 secs timeout, bind as TX, send SMS with delivery receipt via TX, receive delivery receipt via RX.
<pre>
$ ./smppload -Brx --delivery_timeout=5000
INFO:  Connected to 127.0.0.1:2775
INFO:  Bound to Funnel
INFO:  Receipt: [{short_message,"id:22035 submit date:1505071451 done date:1505071451 stat:DELIVRD"},
                 {sm_default_msg_id,0},
                 {data_coding,0},
                 {replace_if_present_flag,0},
                 {registered_delivery,0},
                 {validity_period,[]},
                 {schedule_delivery_time,[]},
                 {priority_flag,0},
                 {protocol_id,0},
                 {esm_class,4},
                 {destination_addr,"375296543210"},
                 {dest_addr_npi,1},
                 {dest_addr_ton,1},
                 {source_addr,"375296660001"},
                 {source_addr_npi,1},
                 {source_addr_ton,1},
                 {service_type,[]},
                 {receipted_message_id,"22035"},
                 {message_state,2}]
INFO:  Stats:
INFO:     Send success:     0
INFO:     Send fail:        0
INFO:     Delivery success: 1
INFO:     Delivery fail:    0
INFO:     Incomings:        0
INFO:     Errors:           0
INFO:     Avg Rps:          0 mps
INFO:  Unbound
</pre>

<pre>
$ ./smppload -Btx -d375296543210 -bHello -D
INFO:  Connected to 127.0.0.1:2775
INFO:  Bound to Funnel
INFO:  Stats:
INFO:     Send success:     1
INFO:     Send fail:        0
INFO:     Delivery success: 0
INFO:     Delivery fail:    0
INFO:     Incomings:        0
INFO:     Errors:           0
INFO:     Avg Rps:          28 mps
INFO:  Unbound
</pre>

* Bind as RX with 5 secs timeout and receive an incoming message.
<pre>
$ ./smppload -Brx -v --delivery_timeout=5000
INFO:  Connected to 127.0.0.1:2775
INFO:  Bound to Funnel
INFO:  Incoming: [{short_message,"Hello"},
                  {sm_default_msg_id,0},
                  {data_coding,0},
                  {replace_if_present_flag,0},
                  {registered_delivery,0},
                  {validity_period,[]},
                  {schedule_delivery_time,[]},
                  {priority_flag,0},
                  {protocol_id,0},
                  {esm_class,0},
                  {destination_addr,"375296660001"},
                  {dest_addr_npi,1},
                  {dest_addr_ton,1},
                  {source_addr,"357291110000"},
                  {source_addr_npi,1},
                  {source_addr_ton,1},
                  {service_type,[]}]
INFO:  Stats:
INFO:     Send success:     0
INFO:     Send fail:        0
INFO:     Delivery success: 0
INFO:     Delivery fail:    0
INFO:     Incomings:        1
INFO:     Errors:           0
INFO:     Avg Rps:          0 mps
INFO:  Unbound
</pre>

## Versioning

**smppload** is versioned according to [semantic versioning](http://semver.org/).

## Known issues and limitations

* Randomly generated message body encoding is Latin1.
* Message body encoding from files or command line is expected to be in UTF-8.
