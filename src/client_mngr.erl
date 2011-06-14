%%%-------------------------------------------------------------------
%%% File    : client_mngr.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : manages the smtp_client nodes and tells the smtp_server
%%%    which client to send the email to, it collects stats from the clients
%%%    and uses them for load balancing
%%%
%%% Created :  9 Feb 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------
-module(client_mngr).

-behaviour(gen_server).

-include("metis.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, call/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {clients=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    register(client_mngr, self()),
    Clients=?CONF(main, clients),
    ?info("Clients to be started:~p", [Clients]),
    ClientRecords=start_clients(Clients),
    {ok, _TimerRef}=timer:send_interval(2*60*1000, {client_mngr, send_stats}),
    process_flag(trap_exit, true),
    {ok, #state{clients=ClientRecords}}.


handle_call(get_total_queue_len, _From, State=#state{clients=ClientList}) ->
    Reply=lists:foldl( fun(#client{statistics={_Load, QueueLen}},Sum)-> QueueLen+Sum end, 0, ClientList ),
    {reply, Reply, State};



handle_call(get_clientlist, _From, State) ->
    ClientNames=?CONF(main, clients),
    AliveClients=State#state.clients,
    AllClients=
	lists:map(fun(ClientName)->
			  case lists:keyfind(ClientName, 2, AliveClients) of
			      false ->
				  #client{client_name=ClientName,
					  relay_to=?CONF(ClientName, relay_to),
					  relay_from=?CONF(ClientName, relay_from)};
			      Client ->
				  Client
			  end
		  end, ClientNames),

    Reply = {clientlist, AllClients},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

%% client requesting config data while coming up
%% ClientPid here is the client_boot:start/1 process
handle_info({ClientName, ClientPid, req_conf}, State) ->
    ?info("Client ~p is requesting configuration",[ClientName]),
    %% looks like node is up, we need to start monitoring it
    monitor_node(ClientName, true),

    ClientArgs=?CONF_SECTION(ClientName),
    ClientPid ! {client_mngr, self(), clientargs, ClientArgs},
    {noreply, State};

%% client is ready
%% ClientPid is the queue manager pid that's running on the node
handle_info({ClientName, ClientPid, ready}, State) ->
    ?info("Queue Manager on ~p is ready",[ClientName]),

    erlang:monitor(process, ClientPid),

    %% update client in the list of clients with the new pid
    ClientList=State#state.clients,
    ClientRecord=lists:keyfind(ClientName, 2, ClientList),
    NewClientRecord=ClientRecord#client{pid=ClientPid},
    NewClientList=lists:keyreplace(ClientName, 2, ClientList, NewClientRecord),

    smtp_server ! {client_mngr, clientlist, NewClientList},
    NewState=State#state{clients=NewClientList},
    {noreply, NewState};

handle_info({ClientName, ClientPid, stats, Stats}, State) ->
    ?info("Stat received from ~p : load ~p, queue length ~p ",[ClientName, element(1,Stats), element(2,Stats)]),

    %% update client in the list of clients with the new stats
    ClientList=State#state.clients,
    ClientRecord=lists:keyfind(ClientName, 2, ClientList),
    NewClientRecord=ClientRecord#client{statistics=Stats},
    NewClientList=lists:keyreplace(ClientName, 2, ClientList, NewClientRecord), 

    smtp_server ! {client_mngr, clientlist, NewClientList},
    NewState=State#state{clients=NewClientList},
    {noreply, NewState};

%% need to log into maillog (riak)
handle_info({ClientName, ClientPid, log, RiakLogEntry}, State) ->
    stat_mngr:notify(RiakLogEntry),
    {noreply, State};

%% msg from itself (client_mngr) to send stats to smtp_server
handle_info({client_mngr, send_stats}, State)->
    smtp_server ! {client_mngr, clientlist, State#state.clients},
    {noreply, State};    

%% sy requesting the clientlist  (this would be a yaws process from the gui node)
handle_info({SenderPid, get_clientlist}, State)->
    ClientNames=?CONF(main, clients),
    AliveClients=State#state.clients,
    AllClients=
	lists:map(fun(ClientName)->
			  case lists:keyfind(ClientName, 2, AliveClients) of
			      false ->
				  #client{client_name=ClientName,
					  relay_to=?CONF(ClientName, relay_to),
					  relay_from=?CONF(ClientName, relay_from)};
			      Client ->
				  Client
			  end
		  end, ClientNames),

    SenderPid ! {client_mngr, clientlist, AllClients},
    {noreply, State};    

handle_info(_Info, State) ->
    ?info("UNKNOWN MSG:~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    Clients=?CONF(main, clients),
    stop_clients(Clients),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%% @todo:   this needs to check if the node is already running before trying to restart it, 
%% if it is then needs to find out the pid and  set up the #client{} record accordingly
start_clients(Clients)->
    StartOne=fun(ClientName)->
		     [Name, Host]=string:tokens(atom_to_list(ClientName), "@"),
		     ClientLocalFQDN=?CONF(ClientName, local_fqdn),
		     Local=?CONF(node(), local_fqdn),
		     Cookie=erlang:get_cookie(),
		     BaseCmd = lists:concat([
					     "erl -detached",
%%					     "erl ",
					     " -name ", ClientName, 
					     " -setcookie ", Cookie,
					     " -pa ebin/ lib/*/ebin deps/*/ebin deps/*/deps/*/ebin",
%%					     " -noinput",
%%					     " -stdlib restricted_shell restricted_shell_callback",
%%					     " +Bc"                        %% shell doesn't exit with ctrl+c
					     " -boot start_sasl"
					     " -config priv/metis_client"
					     " -s client_boot start ", node()]),
		     Cmd=case Host of
			     Local -> BaseCmd;
			     _     -> lists:concat(["rsh ", Host, " 'cd metis/; ", BaseCmd, "'"])
			 end,
		     Xterm=lists:concat(["xterm",
		      			 " -geometry 135x24",
		      			 " -title ", atom_to_list(ClientName),
		      			 " -e "]),
		     Port=open_port({spawn, Cmd}, []),
		     ?debug("Opened port to start client: ~p~n", [Port]),
		     Convert=fun(Addr)->
				     case string:tokens(to_list(Addr), "@") of
					 [HostName]           -> {"", HostName};
					 [UserName, HostName] -> {UserName, HostName}
				     end
			     end,
		     RelayFrom = lists:map(Convert, ?CONF(ClientName, relay_from)),
		     RelayTo   = lists:map(Convert, ?CONF(ClientName, relay_to)++[ClientLocalFQDN]),
		     #client{client_name=ClientName, relay_from=RelayFrom, relay_to=RelayTo}
	     end,
    lists:map(StartOne, Clients).
			     

-spec stop_clients/1 :: ([ClientNames :: atom()]) -> 
				[ Result :: any() | {badrpc, Reason :: term()}] .
stop_clients(Clients)->
    StopOne=fun(ClientName)->
		    case rpc:call(ClientName, init, stop, [], 60000) of
			ok    -> ?debug("Stopped client node: ~p", [ClientName]);
			Error -> ?debug("Could not stop client node: ~p~nReason: ~p", [ClientName, Error])
		    end
	    end,
    lists:map(StopOne, Clients).
			     
call(Request)->
    gen_server:call(client_mngr, Request).


to_list(X) when is_atom(X)->atom_to_list(X);
to_list(X) when is_list(X)->X;
to_list(_) -> error.    
