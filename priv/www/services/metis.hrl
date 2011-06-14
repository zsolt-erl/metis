%%-define( ENV(Par), element(2, application:get_env(hg,Par) ) ).

-define( ENV(Par), conf:get(server, Par) ).    %% this is left here otherwise I'd have to rewrite it in every module
-define( CONF(Section, Key), conf:get(Section, Key) ).
-define( CONF_SECTION(Section), conf:getSection(Section) ).
-define( PGV(Name, Options), proplists:get_value(Name, Options) ).
-define( PL(Name, Options), proplists:lookup(Name, Options) ).



%% queue control record
-record(qc, {id, 
	     from = "", 
	     receiver = "", 
	     submit_time=erlang:localtime(), 
	     try_count=0, 
	     retry_time=erlang:localtime(),
	     status,
	     opaque    %% any additional data that does not change msg processing but can be queried for monitoring queue or analyzing logs
	    }).

-record( counter, {hourly=0, daily=0, total=0, change_time} ).

-record( domainctrl, {dmid,
		      regexp,            %% regexp to match recipient email address  (typically a domain name)
		      cregexp,           %% compiled regexp
		      daily_limit,       %% daily sending limit
		      hourly_limit,      %% hourly limit
		      max_conn,          %% max parallel connections to this domain
		      msg_per_conn,      %% max messages per connection
		      relay_type,        %% pooled|direct    pooled means several messages over one connection
		      c_open_conn=0,     %% counter for open connections
		      c200 = #counter{}, %% counter for sent messages
		      c4xx = #counter{}, %% counter for softbounce messages
		      c5xx = #counter{}, %% counter for hardbounce messages
		      host_msg_queue = queue:new(), %% queue for storing last 10 messages from host after sending
		      state = deliver    %% state of the domain:: atom()=deliver|hold
		     }).

-record(client, {client_name,
		 pid=none,
		 relay_from=[],         %% [{UserName, HostName}]      UserName can be ""
		 relay_to=[],           %% [{UserName, HostName}]
		 statistics={0,0}       %% {Avg5, QueueLength}
		}).



%% value ::  [Entry :: entry()]
-type(qc() :: #qc{}).
-record(riaklogentry, { node   = node()           :: atom(),
			module = ?MODULE          :: atom(),
			time = erlang:localtime() :: tuple(),  %% datetime
			type                      :: sent | queued | deleted | temporary_failure | permanent_failure | network_failure | any() ,
			message                   :: term(),
			qc                        :: qc(),
                        data                      :: term()
		      }).


%% macros to create parametarized record access modules
%% usage:  (?MRIAKLOGENTRY):get(node, #riaklogentry{})
%%         (?MRIAKLOGENTRY):set(node, #riaklogentry{}, example.org)
-define(MRIAKLOGENTRY, recmod:new(riaklogentry, record_info(fields, riaklogentry))).
-define(MQC, recmod:new(qc, record_info(fields, qc))).




-define( debug(Format, Args), log4erl:debug("[~p] :: "++Format, [?MODULE|Args]) ).
-define( info(Format, Args), log4erl:info("[~p] :: "++Format, [?MODULE|Args]) ).
-define( warn(Format, Args), log4erl:warn("[~p] :: "++Format, [?MODULE|Args]) ).
-define( error(Format, Args), log4erl:error("[~p] :: "++Format, [?MODULE|Args]) ).

-define( mdebug(Module, Format, Args), log4erl:debug("[~p] :: "++Format, [Module|Args]) ).
-define( minfo(Module, Format, Args), log4erl:info("[~p] :: "++Format, [Module|Args]) ).
-define( mwarn(Module, Format, Args), log4erl:warn("[~p] :: "++Format, [Module|Args]) ).
-define( merror(Module, Format, Args), log4erl:error("[~p] :: "++Format, [Module|Args]) ).
