%%% File    : queue_mngr.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Created :  7 Nov 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

%%% @doc Queue manager, similar to the queue runner of sendmail
%%%
%%% Scans the queue and sends the messages according to the domain cntrl state.
%%% Manages the connection mngr processes.

-module(queue_mngr).

-compile(export_all).

-include("metis.hrl").

-record(state, {mode=idle, 
		qrunTimerRef, 
		hourlyTimerRef, 
		dailyTimerRef, 
		connmngrs=[], 
		currentmsg=none, 
		loopdelay=10,
		options}).

-define(MSG_QUEUE_LEN, 10).

-define(DEFAULT_OPTIONS, [
			  {host,       localhost},
			  {ip,         {127,0,0,1}},
			  {local_fqdn, smtp_util:guess_FQDN()},
			  {relay_from, []},
			  {relay_to,   []},
			  {queue_dir,  "priv/queue/"},
			  {aliases,    []},
			  {queue_run_interval, 10},
			  {max_try,             8},
			  {aging_basis,		5},
			  {max_msg,           100},
			  {max_conn_time,      60}
			 ]).

-export([start_link/1]).
-export([init/2]).
-export([system_continue/3, system_terminate/4, write_debug/3]).

start_link(OptionsFromConf) ->
    proc_lib:start_link(?MODULE, init, [self(), OptionsFromConf]).


init(Parent, OptionsFromConf) ->
    ?debug("init started, options received:~n~p",[OptionsFromConf]),
    register(queue_mngr, self()),
    Deb = sys:debug_options([]),

    Options = lists:ukeymerge(1, lists:sort(OptionsFromConf), lists:sort(?DEFAULT_OPTIONS)),
    ?debug("Options merged with default:~n~p",[Options]),
    ok=init_queue_control(Options),
    ok=init_domain_control(Options),

    ?debug("Starting timers",[]),
    %% set queue run timer
    {ok, QRunTimerRef}=bigtimer:send_interval(?PGV(queue_run_interval, Options)*1000, {timer, qrun}),
    ?debug("Started queue run timer", []),

    %% set hourly timer
    {ok, HourlyTimerRef}=bigtimer:apply_interval(1000*60*60, ?MODULE, zero_counters_hourly, []),
    %% wait 3 sec so that the daily timer does not coincide with the hourly
    receive
    after 3000->ok
    end,
    %% set daily timer
    {ok, DailyTimerRef}=bigtimer:apply_interval(1000*60*60*24, ?MODULE, zero_counters_daily, []),
    ?debug("Started hourly, daily timers",[]),
    State=#state{ qrunTimerRef=QRunTimerRef,
		  hourlyTimerRef = HourlyTimerRef,
		  dailyTimerRef  = DailyTimerRef,
		  options=Options},

    {ok, _}=bigtimer:send_interval(10*1000, {queue_mngr, report_stats}),

    proc_lib:init_ack(Parent, {ok, self()}),

    MngrPid = ?PGV(mngrpid, Options),
    MngrPid ! {node(), self(), ready},

    ?debug("Entering receive loop", []),
    loop(State, Parent, Deb).

    

init_queue_control(Options)->
    case lists:member(qc, mnesia:system_info(tables)) of
	true->
	    mnesia:clear_table(qc),
	    ok;
	false ->
	    {atomic, ok}=mnesia:create_table(qc, [{attributes, record_info(fields, qc)}])
    end,
    ?debug("mnesia table is ready",[]),
    ?info("reading queue",[]),
    Trans=
	fun()->
		filelib:fold_files(
		  ?PGV(queue_dir, Options), "^qcf.*", 
		  false,
		  fun(FileName, {CRead, CFailed})->
			  case file:read_file(FileName) of
			      {ok, BinRec}->
				  %%Qid=[_,_,_,_ | FN]=(binary_to_term(BinRec))#qc.id,
				  Qid=(binary_to_term(BinRec))#qc.id,

				  QdfFile=?PGV(queue_dir, Options)++"/qdf"++Qid,

				  case filelib:is_file(QdfFile) of 
				      true -> 
					  mnesia:write( binary_to_term(BinRec) );
				      false -> 
					  %% there's no matching data file therefore this control file gets deleted
					  io:format("delete:"),
					  file:delete(FileName)
				  end,
				  io:format("~p ",[CRead+1]),
				  {CRead+1, CFailed};
			      {error, Reason}->
				  ?warn("Could not read queue control file: ~p, reason:",[FileName, Reason]),
				  {CRead, CFailed+1}
			  end
		  end, 
		  {0,0})
	end,
    {atomic, {CRead, CFailed}}=mnesia:transaction(Trans),
    ?info("Scanned queue, read:~p, failed:~p", [CRead, CFailed]),
    ok.


init_domain_control(Options)->
    case lists:member(domainctrl, mnesia:system_info(tables)) of
	true->
	    mnesia:clear_table(domainctrl);
	false ->
	    ?info("creating domainctrl table", []),
	    {atomic, ok}=mnesia:create_table(domainctrl, [{attributes, record_info(fields, domainctrl)}])
    end,

    DomainCtrl=?PGV(domain_control, Options),
    ?info("initializing domain control", []),
    Trans=
	fun()->lists:foldl(
		 fun(Domain, Count)->
			 RE=proplists:get_value(domain, Domain),
			 {ok, CRE}=re:compile(RE),
			 Record=
			     #domainctrl{dmid=Count,
					 regexp        = RE,
					 cregexp       = CRE,
					 daily_limit   = proplists:get_value(daily_limit, Domain, 2000),
					 hourly_limit  = proplists:get_value(hourly_limit, Domain, 1000),
					 max_conn      = proplists:get_value(max_conn, Domain, 10),
					 msg_per_conn  = proplists:get_value(msg_per_conn, Domain, 100),
					 relay_type    = proplists:get_value(relay_type, Domain, direct)},
			 mnesia:write(Record),
			 Count+1
		 end, 0, DomainCtrl)
	end,
    {atomic, DomainCount}=mnesia:transaction(Trans),
    ?info("initialized ~p domains", [DomainCount]),
    ok.




loop(State, Parent, Deb) ->
    ClientMngrPid=?PGV(mngrpid, State#state.options),
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

	%% switching to queue run mode
	{timer, qrun}->
	    loop(State#state{mode=qrun}, Parent, Deb);

	{SMTP_Server, save_into_queue, {QRecord, Data}}->
	    save_into_queue(QRecord, Data, ?PGV(queue_dir, State#state.options)),
	    SMTP_Server ! {self(), queued, QRecord#qc.id},
	    loop(State, Parent, Deb);

	%% a conn manager is reporting that it is idle
	{Pid, conn_mngr, DMID, idle}->
	    NewConnMngrs=change_conn_mngr_state(Pid, DMID, State#state.connmngrs, idle),
	    NewState=State#state{connmngrs=NewConnMngrs},
	    loop(NewState, Parent, Deb);

	%% a conn manager exited
	{'DOWN', _Ref, process, Pid, Reason}->
	    ?debug("conn mngr ~p exited, reason:~p", [Pid, Reason]),

	    %% TAKE PID OFF THE LIST OF CONN MANAGERS
	    MngrLists=State#state.connmngrs,

	    %% Mngrs is the conn manager list for DMID  (Mngrs= [{Pid, busy|idle}])
	    %% F returns true if Pid is not on the list of managers for DMID
	    F=fun({DMID, Mngrs})->not proplists:is_defined(Pid, Mngrs) end,

	    NewState=
		case lists:dropwhile(F, MngrLists) of 
		    [] ->
			?warn("Conn. manager ~p exited but it is not on the list. Did queuemngr crash recently?", [Pid]),
			State;
		    [{DMID, Mngrs}|_] ->
			case mnesia:dirty_read(domainctrl, DMID) of
			    [DM] ->
				%% take conn mngr pid off the list
				NewMngrs=proplists:delete(Pid, Mngrs),
				NewMngrLists=lists:keyreplace(DMID, 1, MngrLists, {DMID, NewMngrs}),
				NewCounter=length(NewMngrs),
				{NewDMState, NewQueue}=
				    case Reason of
					{error, FailureType, Host, Msg} ->
					    %% put domain on hold if conn mngr could not connect to it
					    {hold, flqin({"CM-"++misc:datetime_to_string(erlang:localtime()), Host, Msg}, DM#domainctrl.host_msg_queue, 
							 ?MSG_QUEUE_LEN)};
					_Other ->
					    %% don't change state and msg_queue
					    {DM#domainctrl.state, DM#domainctrl.host_msg_queue}
				    end,
				NewDM=DM#domainctrl{state=NewDMState, host_msg_queue=NewQueue, c_open_conn=NewCounter},
				mnesia:dirty_write(NewDM),
				State#state{connmngrs=NewMngrLists};
			    Else -> 
				?error("unexpected result from mnesia:~p", [Else]),
				State
			end
		end,
	    loop(NewState, Parent, Deb);


	{smtp_server, send_request, QIDs}->
	    lists:foreach(fun(QID)->send_email_from_queue(QID, nonexdom, State) end, QIDs),
	    loop(State, Parent, Deb);

	%% @todo verify, take out - this was for the local delivery which now sends the same format as other domains (see format right below)
	{qsender, sent, _QID, nonexdom}->
	    loop( decrease_delay(State), Parent, Deb);

	{qsender, sent, QID, DMID, Host}->
	    case mnesia:dirty_read(qc, QID) of
		[QRecord] ->
		    ?info("=== SENT ===:~p to:~p", [QID, QRecord#qc.receiver]),
		    %% UPDATE QUEUE
		    delete_from_queue(QID, State),

		    case DMID of 
			nonexdom -> skip;
			_ ->
			    %% UPDATE DOMAINCTRL TABLE
			    case mnesia:dirty_read(domainctrl, DMID) of
				[DM] ->
				    C200=DM#domainctrl.c200,
				    NewD = C200#counter.daily+1,
				    NewH = C200#counter.hourly+1,
				    NewT = C200#counter.total+1,
				    NewDMState = 
					case (NewD>=DM#domainctrl.daily_limit) or (NewH>=DM#domainctrl.hourly_limit) of
					    true -> hold;
					    false -> DM#domainctrl.state
					end,

				    NewC200=#counter{
				      daily  = NewD,
				      hourly = NewH,
				      total  = NewT,
				      change_time = erlang:localtime()
				     },
				    NewQueue=flqin({misc:datetime_to_string(erlang:localtime()), Host, sent}, DM#domainctrl.host_msg_queue, ?MSG_QUEUE_LEN),
				    NewDM=DM#domainctrl{c200=NewC200, state=NewDMState, host_msg_queue=NewQueue},
				    mnesia:dirty_write(NewDM);
				Else1 -> 
				    ?error("unexpected result from mnesia:~p", [Else1])
			    end

		    end,

		    %% SEND EVENT NOTIFICATION FOR LOGGING
		    ClientMngrPid ! {node(), self(), log, #riaklogentry{type=sent, qc=QRecord}};
		Else2 -> 
		    %% @todo 
		    %% this means it will crash next time it tries to get the next msg from mnesia
		    %% crash is caught though
		    %% I leave it like this for now, it can at least handle the messages that come in b4 it crashes
		    %% but it should be fixed up
		    ?error("1. unexpected result from mnesia:~p QID:~p", [Else2, QID])
	    end,
	    loop(decrease_delay(State), Parent, Deb);

	{qsender, error, FailType, QID, DMID, Host, Msg} when 
	      FailType==temporary_failure;
	      FailType==network_failure ->
	    case mnesia:dirty_read(qc, QID) of
		[QRecord] ->
		    ?info("=== ~p ===:~p~n\tto:~p message:~p", [FailType, QID, QRecord#qc.receiver, Msg]),

		    %% UPDATE QUEUE
		    case QRecord#qc.try_count>=?PGV(max_try, State#state.options) of
			true ->
			    ?info("deleting from queue, reached max_try",[]),
			    %% need to delete from queue
			    delete_from_queue(QID, State),
			    ClientMngrPid ! {node(), self(), log, #riaklogentry{type=deleted, qc=QRecord}};
			false ->
			    NextTryDelay=round(math:pow( ?PGV(aging_basis, State#state.options), QRecord#qc.try_count )),
			    NextTryTime = calendar:gregorian_seconds_to_datetime(
					    calendar:datetime_to_gregorian_seconds(erlang:localtime())+NextTryDelay),
			    mnesia:dirty_write(QRecord#qc{retry_time=NextTryTime, status=FailType}),
			    ?debug("scheduling retry for:~p", [NextTryTime])
		    end,

		    case DMID of 
			nonexdom -> skip;
			_ ->
			    %% UPDATE DOMAINCTRL TABLE
			    case mnesia:dirty_read(domainctrl, DMID) of
				[DM]->
				    %% determine new state based on bounced/sent ratio
				    Sent=(DM#domainctrl.c200)#counter.hourly,
				    Bounced=(DM#domainctrl.c4xx)#counter.hourly, %% + (DM#domainctrl.c5xx)#counter.hourly,
				    NewState=case (Sent+Bounced>50) and ( Bounced/(Sent+0.001) >0.2 ) of
						 true  -> hold;
						 false -> DM#domainctrl.state
					     end,
				    %% increase counters
				    C4xx =DM#domainctrl.c4xx,
				    NewD = C4xx#counter.daily+1,
				    NewH = C4xx#counter.hourly+1,
				    NewT = C4xx#counter.total+1,
				    NewC4xx=#counter{
				      daily  = NewD,
				      hourly = NewH,
				      total  = NewT,
				      change_time = erlang:localtime()
				     },

				    %% update message queue
				    NewQueue=flqin({misc:datetime_to_string(erlang:localtime()), Host, Msg}, DM#domainctrl.host_msg_queue, ?MSG_QUEUE_LEN),

				    NewDM=DM#domainctrl{c4xx=NewC4xx, state=NewState, host_msg_queue=NewQueue},
				    mnesia:dirty_write(NewDM);
				Else2 -> 
				    ?error("unexpected result from mnesia:~p", [Else2])
			    end

		    end,

		    %% SEND EVENT NOTIFICATION
		    ClientMngrPid ! {node(), self(), log, #riaklogentry{type=FailType, message=Msg, qc=QRecord}};
		Else1 -> 
		    ?error("2. unexpected result from mnesia:~p", [Else1])
	    end,
	    loop( decrease_delay(State), Parent, Deb);

	{qsender, error, permanent_failure, QID, DMID, Host, Msg}->
	    case mnesia:dirty_read(qc, QID) of
		[QRecord]->
		    ?info("=== permanent_failure ===:~p~n\tto:~p message:~p", [QID, QRecord#qc.receiver, Msg]),
		    %% UPDATE QUEUE
		    delete_from_queue(QID, State),

		    case DMID of 
			nonexdom -> skip;
			_ ->
			    %% UPDATE DOMAINCTRL TABLE
			    case mnesia:dirty_read(domainctrl, DMID) of
				[DM]->
				    %% determine new state based on bounced/sent ratio
				    Sent=(DM#domainctrl.c200)#counter.hourly,
				    Bounced=(DM#domainctrl.c4xx)#counter.hourly + (DM#domainctrl.c5xx)#counter.hourly,

				    NewState=case (Sent+Bounced>50) and ( Bounced/(Sent+0.001)  > 0.2 ) of
						 true  -> hold;
						 false -> DM#domainctrl.state
					     end,

				    %% increase counters
				    C5xx =DM#domainctrl.c5xx,
				    NewD = C5xx#counter.daily+1,
				    NewH = C5xx#counter.hourly+1,
				    NewT = C5xx#counter.total+1,
				    NewC5xx=#counter{
				      daily  = NewD,
				      hourly = NewH,
				      total  = NewT,
				      change_time = erlang:localtime()
				     },

				    %% update message queue
				    NewQueue=flqin({misc:datetime_to_string(erlang:localtime()), Host, Msg}, DM#domainctrl.host_msg_queue, ?MSG_QUEUE_LEN),
				    NewDM=DM#domainctrl{c5xx=NewC5xx, state=NewState, host_msg_queue=NewQueue},
				    mnesia:dirty_write(NewDM);

				Else1 -> 
				    ?error("unexpected result from mnesia:~p", [Else1])
			    end
		    end,
		    %% SEND EVENT NOTIFICATION
		    ClientMngrPid ! {node(), self(), log, #riaklogentry{type=permanent_failure, message=Msg, qc=QRecord}};
		Else -> 
		    ?error("3. unexpected result from mnesia:~p", [Else])
	    end,
	    loop( decrease_delay(State), Parent, Deb);

	{qsender, error, FailType, QID, DMID, Host, Msg}->
	    case mnesia:dirty_read(qc, QID) of
		[QRecord]->
		    ?info("=== FAILED ===:~p,~n\tto:~p~n   type:~p, error msg:~p", [QID, QRecord#qc.receiver, FailType, Msg]),
			    NextTryDelay=round(math:pow( ?PGV(aging_basis, State#state.options), QRecord#qc.try_count )),
			    NextTryTime = calendar:gregorian_seconds_to_datetime(
					    calendar:datetime_to_gregorian_seconds(erlang:localtime())+NextTryDelay),
			    mnesia:dirty_write(QRecord#qc{retry_time=NextTryTime, status=FailType}),
			    ?debug("scheduling retry for:~p", [NextTryTime]);
		Else -> 
		    ?error("4. unexpected result from mnesia:~p", [Else])
	    end,
	    loop( decrease_delay(State), Parent, Deb);

	{queue_mngr, report_stats}->
	    Stats={ round(100 * (1- (50/(50+cpu_sup:avg5())))), mnesia:table_info(qc, size)},
	    ClientMngrPid ! {node(), self(), stats, Stats},
	    loop(State, Parent, Deb);

	%% this is from the frontend, requesting domainctrl data
	{Pid, get_domainctrl_data}->
	    ?info("DOMAINCTRL DATA REQUEST!",[]),
	    Data=getDomainctrlData(),
	    Pid ! {self(), domainctrl_data, Data},
	    loop(State, Parent, Deb);

	{Pid, get_state}->
	    ?info("STATE REQUEST!",[]),
	    Pid ! {self(), state, State},
	    loop(State, Parent, Deb);
	
	%% DomainCtrl is a proplist, see format in priv/mta.conf.example
	{Pid, init_domain_control, DomainCtrl}->
	    ok=init_domain_control([{domain_control, DomainCtrl}]),
	    Pid ! {self(), domainctrl_initialized},
	    loop(State, Parent, Deb);

	Any ->
	    ?debug("UNEXPECTED MSG:~p", [Any]),
	    loop(State, Parent, Deb)

    after State#state.loopdelay ->
	    %% check what mode we are in
	    NewState=
		case State#state.mode of
		    idle -> State;
		    qrun -> 
			try queue_run_next_step( State )
			catch
			    Type:Exception -> 
				?debug("EXCEPTION CAUGHT: ~p::~p",[Type, Exception]),
				?debug("Stacktrace: ~p",[erlang:get_stacktrace()]),
				?debug("getting the first msg",[]),
				{atomic, FirstQID} = mnesia:transaction(fun()->mnesia:first(qc) end),
				?debug("got first msg",[]),
				State#state{currentmsg=FirstQID}   %% update it with first msg
			end
		end, 
	    loop(NewState, Parent, Deb)
    end.   %% receive

system_continue(Parent, Deb, State) ->
    loop(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).



send_email_from_queue(QID, DMID, State)->
    [QRecord]=mnesia:dirty_read(qc, QID),
    ?debug("sending email to:~p", [QRecord#qc.receiver]),
    TryCount=QRecord#qc.try_count,
    mnesia:dirty_write(QRecord#qc{try_count=TryCount+1, status=sending}),

    FromStr=QRecord#qc.from,
    OriginalRecStr=QRecord#qc.receiver,
    [User, Host]=string:tokens(OriginalRecStr, "@"),
    case Host==?PGV(local_fqdn, State#state.options) of
	true ->
	    %% local delivery through procmail
	    ?debug("local delivery for:~p",[QID]),

	    AliasTable=proplists:get_value(aliases, State#state.options),
	    NewUser=proplists:get_value(User, AliasTable, User),
	    RecStr=NewUser++"@"++Host,

	    [_,_,_,_ | FN]=QID,
	    FileName=?PGV(queue_dir, State#state.options)++"/qdf"++QID,
	    Msg=
		case file:read_file(FileName) of
		    {ok, Data}->
			binary_to_list(Data);
		    {error, _}->
			?debug("Missing data file for queue id:~s",[QID]),
			"[metis]: missing data file"
		end,

	    case misc:exec("/usr/bin/procmail -d "++RecStr, Msg) of
	     	[]->  
	     	    queue_mngr ! {qsender, sent, QID, DMID, Host};
	     	ProcmailError -> 
	     	    ?error("=== Procmail Error ===:~p~n\tto:~p message:~p", [ProcmailError, QID, QRecord#qc.receiver]),
	     	    queue_mngr ! {qsender, error, permanent_failure, QID, DMID, Host, ProcmailError}
	    end;
	false ->
	    RelayHost=case ?PGV(smartrelay_host, State#state.options) of
			  undefined -> Host;
			  SmartHost -> SmartHost
		      end,
	    SendingOptions=[{relay, RelayHost}, ?PL(queue_dir, State#state.options), ?PL(ip, State#state.options)],
	    spawn(queue_smtp_client,send, [{FromStr, OriginalRecStr, QID, DMID}, SendingOptions])
    end.
    


%% delete_from_queue(QID, State)->
%%     mnesia:dirty_delete(qc, QID),
%%     case file:delete(?PGV(queue_dir, State#state.options)++"/qcf"++QID) of
%% 	ok->	%% check for any other qcf file, if there's no more then delete qdf
%% 	    Ref=string:substr(QID, 5),
%% 	    case filelib:wildcard(?PGV(queue_dir, State#state.options)++"/qcf????"++Ref) of
%% 		[]->
%% %%		    file:delete(?PGV(queue_dir, State#state.options)++"/qdf"++Ref);
%% 		    %% @todo this breaks the functionality of having more then one qdf per qcf (one email w/several recipients)  FIX IT!!
%% 		    file:delete(?PGV(queue_dir, State#state.options)++"/qdf"++QID);
%% 		_FileNames ->
%% 		    ok
%% 	    end;
%% 	{error, Reason} ->
%% 	    ?warn("Could not delete sent file from queue, qid:~p, reason:~p",[QID, Reason])
%%     end.



delete_from_queue(QID, State)->
    mnesia:dirty_delete(qc, QID),
    QueueDir=?PGV(queue_dir, State#state.options),
    case file:delete(QueueDir++"/qcf"++QID) of
	ok              -> ok;
	{error, Reason1} ->
	    ?warn("Could not delete queue control file from queue_dir, qid:~p, reason:~p",[QID, Reason1])
    end,
    case file:delete(QueueDir++"/qdf"++QID) of
	ok              -> ok;
	{error, Reason2} ->
	    ?warn("Could not delete queue data file from queue_dir, qid:~p, reason:~p",[QID, Reason2])
    end.


    
find_matching_domain(Domain)->
    {atomic, FirstKey} = mnesia:transaction(fun()->mnesia:first(domainctrl) end),
    find_matching_domain(Domain, FirstKey).
    
find_matching_domain(_Domain, '$end_of_table')->
    nomatch;
find_matching_domain(Domain, Key) ->
    {atomic, [DomainRecord]}=mnesia:transaction(fun()->mnesia:read(domainctrl, Key) end),

    case re:run(Domain, DomainRecord#domainctrl.cregexp) of
	{match, _} -> DomainRecord;
	nomatch    -> 
	    {atomic, NextKey} = mnesia:transaction(fun()->mnesia:next(domainctrl, Key) end),
	    find_matching_domain(Domain, NextKey)
    end.


change_conn_mngr_state(Pid, DMID, Connmngrs, NewState)->
    NewListForDMID=
	case lists:keyfind(DMID, 1, Connmngrs) of
	    {DMID, ManagersList}->
		{DMID, lists:keystore(Pid, 1, ManagersList, {Pid, NewState})};
	    false ->
		?debug("change_conn_mngr_state: adding new conn manager for DMID ~p with state:~p",[DMID, NewState]),
		{DMID, [{Pid, NewState}]}
	end,
    NewConnMngrs=lists:keystore(DMID, 1, Connmngrs, NewListForDMID).


%% fixed lenght queue in
%% if queue grows longer then Length the first item (oldest) gets dropped
flqin(Item, Queue, Length) ->
  case queue:is_queue(Queue) of
      true ->
	  Q2=queue:in(Item, Queue),
	  case queue:len(Q2)>Length of
	      false -> 
		  Q2;
	      true  ->
		  {{value, _ItemOut}, Q3}=queue:out(Q2),
		  Q3
	  end;
      false ->
	  not_queue
  end.


queue_run_next_step(State)->
    CurrentMsg=State#state.currentmsg,
    %% check if there's a msg to send
    case CurrentMsg of
	none ->
	    ?debug("QUEUE RUN STARTED",[]),
   	    {atomic, QID} = mnesia:transaction(fun()->mnesia:first(qc) end),
	    State#state{currentmsg=QID};
	'$end_of_table' ->
	    ?debug("QUEUE RUN ENDED",[]),
	    State#state{mode=idle, currentmsg=none};
	_CurrentMsg ->
	    {atomic, NextQID}   = mnesia:transaction(fun()->mnesia:next(qc, CurrentMsg) end),
	    [QRecord] = mnesia:dirty_read(qc, CurrentMsg),
	    RetryTime=QRecord#qc.retry_time,
	    MsgStatus=QRecord#qc.status,   
	    
	    %% @todo rewrite! this is ugly !!  too many levels !!
	    %% check if we reached the RetryTime of the msg
	    NState=
		case (RetryTime>erlang:localtime()) or (MsgStatus==sending) of
		    true ->
			%% cannot send it
			State;
		    false ->
			ToStr=QRecord#qc.receiver,
			[User, Domain]=string:tokens(ToStr, "@"),
			%% check if Domain is in the domaincntrl table
			case find_matching_domain(Domain) of
			    nomatch ->        %% we are not controlling this domain so the email can be sent
				?debug("NO MATCHING DOMAIN",[]),
				send_email_from_queue(CurrentMsg, nonexdom, State),
				State;
			    DM ->             %% controlled domain, check if ok to send
				case DM#domainctrl.state of
				    deliver ->     %% at this point the email can be sent, need to decide how to send it
					case DM#domainctrl.relay_type of
					    direct ->
						mnesia:dirty_write(QRecord#qc{status=sending}),
						send_email_from_queue(CurrentMsg, DM#domainctrl.dmid, State),
						State;
					    pooled ->
						DMID=DM#domainctrl.dmid,
						%% choose conn manager to use
						DMConnManagers=proplists:get_value(DMID, State#state.connmngrs,[]),
						Connmngrs=State#state.connmngrs,
						%% find idle conn manager
						case lists:keyfind(idle, 2, DMConnManagers) of
						    false ->
							%% no idle conn manager, check if we can create a new one
							case length(DMConnManagers)<DM#domainctrl.max_conn of
							    true->   %% create new one
								RelayHost=case ?PGV(smartrelay_host, State#state.options) of
									      undefined -> Domain;
									      SmartHost -> SmartHost
									  end,
								ConnMngrOptions=[{relay, RelayHost}, 
										 ?PL(queue_dir, State#state.options), 
										 ?PL(ip, State#state.options),
										 {max_msg, DM#domainctrl.msg_per_conn}, 
										 ?PL(max_conn_time, State#state.options)],

								{Pid, _MonitorRef}=conn_mngr:start_monitor(DMID, ConnMngrOptions),
								NewDMConnMngrs=[{Pid, busy}|DMConnManagers],
								?debug("New DM Conn Mngrs DMID:~p Mngr:~p",[DMID, NewDMConnMngrs]),
								mnesia:dirty_write(DM#domainctrl{c_open_conn=length(NewDMConnMngrs)}),
								Pid ! {queue_mngr, send, CurrentMsg},
								NewConnMngrs=lists:keystore(DMID, 1, Connmngrs, {DMID, NewDMConnMngrs}),
								State#state{connmngrs=NewConnMngrs};
							    false ->
								State   %% cannot create new one, we don't do anything with the msg
							end;
						    {Pid, idle} ->
							%% tell conn mngr to send it
							Pid ! {queue_mngr, send, CurrentMsg},
							mnesia:dirty_write(QRecord#qc{status=sending}),
							NewDMConnMngrs=lists:keystore(Pid, 1, DMConnManagers, {Pid, busy}),
							NewConnMngrs=lists:keystore(DMID, 1, Connmngrs, {DMID, NewDMConnMngrs}),
							State#state{connmngrs=NewConnMngrs}
						end %% case lists:keyfind(idle, 2, ConnManagers) of
					end;   %% case DM#domainctrl.relay_type of
				    hold ->  %% we are not sending the email
					State
				end %% case DM#domainctrl.state of 
			end %% case find_matching_domain(Domain) of
		end,  %% case RetryTime=<erlang:localtime() of
	    NState#state{currentmsg=NextQID}   %% update it with next msg
    end. %% case State#state.currentmsg of


zero_counters_hourly()->
    Transaction=
	fun()->
		mnesia:foldl(
		  fun(Record=#domainctrl{regexp=Domain,
					 state=State,
					 daily_limit=LD,
					 hourly_limit=LH,
					 c200= C200,
					 c4xx= C4xx,
					 c5xx= C5xx},
		      Acc)->
			  
			  %% check if the domain is on hold and if we can put it back to deliver 
			  NewState=
			      case (State==hold) and (C200#counter.daily<LD) of
				  true -> 
				      ?info("Domain: ~p changed state to 'deliver'", [Domain]),
				      deliver;
				  false ->
				      State
			      end,
			  Write=
			      fun()->
				      mnesia:write(Record#domainctrl{
						     c200  = C200#counter{hourly=0, change_time=erlang:localtime()},
						     c4xx  = C4xx#counter{hourly=0, change_time=erlang:localtime()},
						     c5xx  = C5xx#counter{hourly=0, change_time=erlang:localtime()},
						     state = NewState})
			      end,
			  mnesia:transaction(Write),
			  Acc+1
		  end, 0, domainctrl)
	end,
    {atomic, _RecCount}=mnesia:transaction( Transaction ),
    ok.


zero_counters_daily()->
    Transaction=
	fun()->
		mnesia:foldl(
		  fun(Record=#domainctrl{regexp=Domain,
					 state=State,
					 c200= C200,
					 c4xx= C4xx,
					 c5xx= C5xx},
		      Acc)->
			  
			  %% check if the domain is on hold and if we can put it back to deliver 
			  NewState=
			      case (State==hold) of
				  true -> 
				      ?info("Domain: ~p changed state to 'deliver'", [Domain]),
				      deliver;
				  false ->
				      State
			      end,
			  Write=
			      fun()->
				      mnesia:write(Record#domainctrl{
						     c200  = C200#counter{daily=0, change_time=erlang:localtime()},
						     c4xx  = C4xx#counter{daily=0, change_time=erlang:localtime()},
						     c5xx  = C5xx#counter{daily=0, change_time=erlang:localtime()},
						     state = NewState})
			      end,
			  mnesia:transaction(Write),
			  Acc+1
		  end, 0, domainctrl)
	end,
    {atomic, _RecCount}=mnesia:transaction( Transaction ),
    ok.


increase_delay(State)->
    D=State#state.loopdelay,
    D2=if
	   (D<1000)-> D+2;
	   true    -> 1000
       end,
    ?debug("Increased loop delay to:~p", [D2]),
    State#state{loopdelay=D2}.
	       
decrease_delay(State)->
    D=State#state.loopdelay,
    D2=if
	   (D>10) -> D-2;
	   true   -> 10
       end,
    ?debug("Decreased loop delay to:~p", [D2]),
    State#state{loopdelay=D2}.


%% @todo rewrite to handle multiple TOs  (same email going to several addresses)
%% send_to_client in the smtp server needs to be changed also

save_into_queue(QRecord, Data, QueueDir)->
    ?debug("saving into queue",[]),
    %%   insert data in queue table
    
    QueueControlFile=term_to_binary(QRecord),
    FileName = QueueDir++"/qcf"++QRecord#qc.id,

    ?debug("writing qcf file: ~p",[FileName]),

    file:write_file(FileName, QueueControlFile),
    Write=fun()->mnesia:write(QRecord) end,
    {atomic, ok}=mnesia:transaction(Write),
    DataFileName=QueueDir++"/qdf"++QRecord#qc.id,

    ?debug("writing qdf file: ~p",[DataFileName]),

    file:write_file(DataFileName, Data).


getDomainctrlData()->
    RecTransform=							
	fun(Record, Acc)-> 
		{counter, C200H, C200D, C200T, _ChangeDate1}=Record#domainctrl.c200,
		C200Str="("++integer_to_list(C200H)++","++integer_to_list(C200D)++","++integer_to_list(C200T)++")",

		{counter, C4XXH, C4XXD, C4XXT, _ChangeDate2}=Record#domainctrl.c4xx,
		C4XXStr="("++integer_to_list(C4XXH)++","++integer_to_list(C4XXD)++","++integer_to_list(C4XXT)++")",

		{counter, C5XXH, C5XXD, C5XXT, _ChangeDate3}=Record#domainctrl.c5xx,
		C5XXStr="("++integer_to_list(C5XXH)++","++integer_to_list(C5XXD)++","++integer_to_list(C5XXT)++")",

		HostMsgQ=lists:flatten(
			   lists:map(fun(X)->io_lib:format("~p~n",[X]) end, 
				     queue:to_list(Record#domainctrl.host_msg_queue))
			  ),
		Transformed=
		    {struct, 
		     [{dmid, Record#domainctrl.dmid},
		      {regexp, Record#domainctrl.regexp},
		      {state, atom_to_list(Record#domainctrl.state)},
		      {daily_limit, Record#domainctrl.daily_limit},
		      {hourly_limit, Record#domainctrl.hourly_limit},
		      {max_conn, Record#domainctrl.max_conn},
		      {msg_per_conn, Record#domainctrl.msg_per_conn},
		      {relay_type, atom_to_list(Record#domainctrl.relay_type)},
		      {c_open_conn, Record#domainctrl.c_open_conn},
		      {c200, C200Str},
		      {c4xx, C4XXStr},
		      {c5xx, C5XXStr},
		      {host_msg_queue, HostMsgQ}]},
		Acc++[Transformed]
	end,

    Transaction=fun()->mnesia:foldl(RecTransform, [], domainctrl) end,

    %% Records contain [{struct, [...]}] that can be converted to JSON 
    {atomic, Records}=mnesia:transaction(Transaction),
    Records.
    
