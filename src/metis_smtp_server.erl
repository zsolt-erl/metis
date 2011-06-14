-module(metis_smtp_server).

-behaviour(gen_smtp_server_session).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
	 handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/5, handle_RSET/1, handle_VRFY/2,
	 handle_other/3, handle_AUTH/4, code_change/3, terminate/2]).

-compile(export_all).

-include("metis.hrl").

-record(state, {options = [], fqdn=smtp_util:guess_FQDN()}).

%% @spec( init(Hostname :: binary(), SessionCount :: non_neg_integer(), Address :: tuple(), Options :: list()) -> 
%% {'ok', string(), #state{}} | {'stop', any(), string()}).
init(Hostname, SessionCount, Address, Options) ->
    ?debug("peer: ~p", [Address]),
    case SessionCount > 100 of
	false ->
	    Banner = io_lib:format("~s PostMaster smtp server", [Hostname]),
	    State = #state{options = Options},
	    {ok, Banner, State};
	true ->
	    ?debug("Connection limit exceeded", []),
	    {stop, normal, io_lib:format("421 ~s is too busy to accept mail right now", [Hostname])}
    end.

-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', pos_integer(), #state{}} | {'ok', #state{}}.
handle_HELO(<<"invalid">>, State) ->
    %% contrived example
    {error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
    {ok, State};
handle_HELO(Hostname, State) ->
    ?debug("HELO from ~s", [Hostname]),
    {ok, 655360, State}. %% 640kb of HELO should be enough for anyone.
%%If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', list(), #state{}}.
handle_EHLO(<<"invalid">>, _Extensions, State) ->
    %% contrived example
    {error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
    ?debug("EHLO from ~s", [Hostname]),
    %% You can advertise additional extensions, or remove some defaults
    MyExtensions = case proplists:get_value(auth, State#state.options, false) of
		       true ->
			   %% auth is enabled, so advertise it
			   Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
		       false ->
			   Extensions
		   end,
    {ok, MyExtensions, State}.

-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_MAIL(From, State) ->
    ?debug("Mail from ~s", [From]),
    %% you can accept or reject the FROM address here
    %% {error, "Not accepting mail from this sender", State}
    {ok, State}.

-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(Extension, State) ->
    ?debug("Mail from extension ~s~n", [Extension]),
    %% any MAIL extensions can be handled here
    {ok, State}.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(To, State) ->
    ?debug("Mail to ~s", [To]),
    %% you can accept or reject RCPT TO addesses here, one per call
    {ok, State}.


-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_RCPT_extension(Extension, State) ->
    %% any RCPT TO extensions can be handled here
    ?debug("Mail to extension ~s", [Extension]),
    {ok, State}.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}, Clients :: list()) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, OriginalTo, Data, State, Clients) ->
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),

    %% check if it's coming from or going to any of my_domains
    case accept_email(From, OriginalTo) of 
	true->
	    RerouteTable=?CONF(node(), reroute),
	    To=lists:map(fun(Addr)->proplists:get_value(Addr, RerouteTable, Addr) end, OriginalTo),
	    
	    case send_to_client(Reference, From, To, Data, Clients, proplists:get_value(instant_relay, State#state.options, [])) of
		{ok, QIDs} ->
		    ?info("=== QUEUED ===: message from ~s to ~p~n\tqueued as ~s, body length ~p", 
			  [From, To, Reference, byte_size(Data)]),
		    {ok, Reference, State};
		error ->
		    ?debug(">>> NOT QUEUED : message from ~s to ~p NOT queued", [From, To]),
		    {error, "500 Invalid message, mimedecode failed.", State}
	    end;
	false -> %% reject it
	    ?info("=== REJECTED ===: message from ~s to ~p", [From, OriginalTo]),
	    {error, "571 Delivery not authorized, message refused", State}
    end.

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
    %% reset any relevant internal state
    State.

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
    %% You can implement other SMTP verbs here, if you need to
    {lists:flatten(io_lib:format("500 Error: command not recognized : '~s'", [Verb])), State}.

%% this callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
-spec handle_AUTH(Type :: 'login' | 'plain' | 'cram-md5', Username :: binary(), Password :: binary() | {binary(), binary()}, #state{}) -> {'ok', #state{}} | 'error'.
handle_AUTH(Type, <<"username">>, <<"PaSSw0rd">>, State) when Type =:= login; Type =:= plain ->
    {ok, State};
handle_AUTH('cram-md5', <<"username">>, {Digest, Seed}, State) ->
    case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
	Digest ->
	    {ok, State};
	_ ->
	    error
    end;
handle_AUTH(_Type, _Username, _Password, _State) ->
    error.

-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
    {ok, Reason, State}.



%%%% Internal Functions %%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		
send_to_client(Reference, From, To, Data, Clients, InstantRelay)->
    try mimemail:decode(Data) of
	{_,_,Headers,_,Body} ->
	    %% Data successfully decoded
	    ?debug("Message decoded successfully!", []),

	    Subject=binary_to_list( proplists:get_value(<<"Subject">>, Headers, <<"no subject">>) ),
	    Opaque=[{subject, Subject}],

	    F=
		lists:foldl(
		  fun(Receiver, {Count, QIDs})->
			  ExtendedReference=string:right(integer_to_list(Count),4,$0)++Reference,
			  {ClientName, ClientPid, _Score, _Load}=choose_client(From, Receiver, Clients),

			  QRecord=#qc{id=ExtendedReference, 
				      from=binary_to_list(From), 
				      receiver=binary_to_list(Receiver), 
				      status=new,
				      opaque=Opaque},

			  ClientPid ! {self(), save_into_queue, {QRecord, Data}},

			  receive 
			      {ClientPid, queued, ExtendedReference} ->
				  stat_mngr:notify(#riaklogentry{type=queued, qc=QRecord}),
				  ?info("Msg:~p from ~p to ~p QUEUED by ~p",[ExtendedReference, From, Receiver, ClientName]),
				  %% check if we need to relay it immediately
				  RelayAddrBinList=[list_to_binary(Addr) || Addr <- InstantRelay],
				  case (To--RelayAddrBinList) of
				      [] ->     %% all the addresses in 'To' were on the 'instant_relay' address list in the callback options
					  ClientPid ! {smtp_server, send_request, QIDs};
				      _ ->
					  dont_send_now
				  end,
				  {Count+1, [ExtendedReference|QIDs]}
			  after 10000->
				  ?warn("Msg:~p from ~p to ~p NOT QUEUED by ~p",[ExtendedReference, From, Receiver, ClientName]),
				  {Count, QIDs}
			  end
		  end, {0, []}, To),
	    case F of 
		{Count, QIDs} ->
		    %% @todo if length(To)=/=length(QIDs) here then some messages did not get queued or at least the client did not answer on time
		    {ok, QIDs};
		error -> error
	    end
    catch
	What:Why ->
	    ?debug("Message decode FAILED with ~p:~p", [What, Why]),
	    error
    end.


choose_client(From, To, Clients)->
    [FromUser, FromHost] = case string:tokens(misc:multistrip( string:to_lower(binary_to_list(From)), "<>"), "@") of
			       [U,H] -> [U,H];
			       _     -> ["", ""]
			   end,
    %% assuming that To address is always correct (User@Host)
    [ToUser, ToHost]=string:tokens(misc:multistrip( string:to_lower(binary_to_list(To)), "<>"), "@"),

    ClientScores=
	lists:map(fun(#client{client_name=ClientName, pid=ClientPid, relay_from=RelayFrom, relay_to=RelayTo, statistics=Load})->
			  Score1=lists:foldl(fun({U, H},Acc) -> 
						     case {U,H} of 
							 {FromUser, FromHost} -> Acc+2;
							 {FromUser, _}        -> Acc+1;
							 {_, FromHost}        -> Acc+1;
							  _                   -> Acc
						     end
					     end, 0, RelayFrom),
			  Score2=lists:foldl(fun({U, H},Acc) ->
						     case {U,H} of 
							 {ToUser, ToHost} -> Acc+2;
							 {ToUser, _}      -> Acc+1;
							 {_, ToHost}      -> Acc+1;
							  _               -> Acc
						     end
					     end, Score1, RelayTo),
			  {ClientName, ClientPid, Score2, Load}
		  end, Clients),
    SortedScores=lists:reverse(lists:keysort(3, ClientScores)),
    HighestScore=element(3, hd(SortedScores)),
    ?debug("Client scores: ~p",[SortedScores]),
    Prospects=lists:takewhile(fun(E)->element(3,E)==HighestScore end, SortedScores),
    %% if there are several prospective clients then decide based on load
    case Prospects of
	[Prospect] -> Prospect;
	List       -> hd(lists:keysort(4, List))
    end.
	    


	
-spec accept_email(From::binary(), To::[binary()]) -> boolean().
%% @doc check if this server accepts this email
accept_email(From, To)->
    MyDomains=?CONF(node(), my_domains),
    MyDomainsMember=fun(Elem)->lists:member(Elem, MyDomains) end,

    FromDomain = case string:tokens(binary_to_list(From), "@") of
		     [_U,D] -> D;
		     _      -> ""
		 end,

    case MyDomainsMember( string:to_lower(FromDomain) ) of
	true -> true;
	false ->
	    ToDomains=lists:map(fun(Addr)->[_,Domain]=string:tokens(binary_to_list(Addr), "@"), Domain end, To),
	    lists:all(MyDomainsMember, ToDomains)
    end.
	
	     
