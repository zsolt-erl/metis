Overview
--------
Metis is a Erlang/OTP Mail Transfer Agent based on the [gen_smtp][] library.  
I couldn't find an open source MTA that was able to do throttling of the outgoing mail so I decided to write one. Mail log analysis was also a problem (busy mail server = huge textfiles as logs) therefore Metis uses Riak as the mail log backend.


Main features

- runs several mail queues on one host or on a cluster of hosts  
  the smtp clients (queues) can run on the same host where the smtp server is running or can be on other hosts  
  any number of clients can run on a host (eg. to bind to different network interfaces)
- distributes the emails between queues based on sender/recipient address or domain
- does load balancing between queues
- throttles outgoing mail on a per queue per domain basis
- has a web interface for traffic monitoring and log analysis


Note:  
This is the first version. There's nothing particularly optimized and I mainly tested it on a cluster of Virtual Machines. I've just started testing it in a production environment.

Installation
------------

### Install dependencies
install erlang  
install riak  (not necessary but currently this is the only maillog backend it can use)  
install procmail
eg. on Ubuntu:

    sudo apt-get install erlang riak procmail

Procmail has to be suid, guid.
This is the default on Ubuntu but not on RedHat, CentOS so on RedHat do:  
    chmod ug+s `which procmail`  


### Clone or Download and Unpack

    git clone git://github.com/zsolt-erl/metis metis

or

    after downloading 
    tar -xvzf metis.tar.gz

### Compile
    cd metis
    make

### Install
    sudo make install

this will create the metis user and set up a full installation in /home/metis/metis
if you are setting up a host that's only a client eg. only manages a queue and sends emails
then do

    sudo make client


### Set up ssh
This is only needed if you are running Metis on a cluster.
The server needs to be able to log in to the clients without a password to start the client nodes

    sudo make keys

this will create a private and a public key  
the private key is in */home/metis/.ssh/id_rsa*  
the public key is in */home/metis/metis/priv/id_rsa.pub*

you need to copy the public key to each client host in the */home/metis/.ssh/authorized_keys* file (either copy and rename or append it to the *authorized_keys* file)

test the connection (try to log in from the server host to the client host)

    su - metis
    metis@server_host> ssh client_host

this should log in to the client without asking for a password

### Edit config files
config files are in /home/metis/priv on the server host

start with mta.conf, this is the main config file  
mta.conf is set up to use aliases.conf, domain.conf and reroute.conf

you should change them to match your hostnames, sending rates, etc.

### Configure riak
edit /etc/riak/vm.args  
append:

    -pa /home/metis/metis/ebin

restart riak if it was already running  (*riak restart* is not enough, you need to stop and start riak: *riak stop; riak start*)  
otherwise start it (*riak start*)



*At this point the installation is done and the server is ready*


Running
-------
Server node needs to be started as root  
Client nodes can be started as whatever user it'll be running as (default: metis)

    su
    su - metis    (on client node)
    cd /home/metis/metis
    ./metis help

Starting the server:

    ./metis start

Stopping the server

    ./metis stop  

Starting / stopping the web gui

    ./metis web-start
    ./metis web-stop

Checking if it's alive

    ./metis ping

Getting server status

    ./metis status

Viewing recent mail log entries

    ./metis log

Web GUI
-------

The Web GUI is available at *http://metis-server-host:8010/gui*  
(8010 is the default port, it can be changed in *priv/mta.conf*)

The GUI shows the amount of

- emails received by the server
- emails sent (all the queues total)
- emails still in the queues
- softbounced / hardbounced emails
- emails deleted from the queues

The graphs for received/sent emails and queue size are updated every minute and show the last hour.
The area below the graphs shows what's happening with each queue. If you click on the nodename of the queue 
a window comes up that shows a per domain breakdown of whats happening in the queue (it only shows domains 
that are controlled by that queue, see *domain_control* key in *mta.conf*). Domains that are not shown on this 
grid can still be routed through this queue however the flow to these domains is not controlled by the queue 
(no throttling, connection pooling). The last field in the grid is the message queue. If you double click on it 
it shows the result of the last 10 attempts to send to the domain.

### Tools menu:
#### Mail Log Cache
The mail log is kept in a cache and gets written into the Riak database every 5min.
This menu shows the current content of the cache.

#### Analyzer
This is a simple log analyzer. It reads the mail log from the Riak database and shows the log entries and 
a graph of the received entries. Set the start and end date and time and click refresh. You can also filter what shows up on the 
grid by entering regular expressions in the filter fields and pressing the refresh button. 
The checkbox next to the filter field groups by that field (similar to *group by* in SQL). The graph shows the first 5 groups 
if you group by a field (actually you can group by several fields at the same time).

example:  
>  filter the sender field    : joe@hotmail.com  
>  filter the recipient field : gmail.com  
>  group by type  (this is the log entry type: queued, sent, softbounced, etc.)  
>  after a refresh you will see the log entries, a grid with the counts for all the groups and the graphs for the first 5 groups  
>  (graphs for queued, sent, softbounced, hardbounced, deleted emails)


[gen_smtp]: http://github.com/Vagabond/gen_stmp