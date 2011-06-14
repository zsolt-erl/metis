%%% File    : conn_mngr.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% @doc connection manager, manages one connection to one destination host
%%%
%%%          sends several emails through the conection and closes the connection after reaching a maximum 
%%%          number of messages or a time limit

%%% Created : 31 Mar 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%
%%% based on gen_smtp_client from Andrew Thompson:
%%%
%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-module(conn_mngr).

-define(DEFAULT_OPTIONS, [
			  {ssl, false}, % whether to connect on 465 in ssl mode
			  {tls, if_available}, % always, never, if_available
			  {auth, if_available},
			  {hostname, smtp_util:guess_FQDN()},
			  {retries, 1} % how many retries per smtp host on temporary failure
			 ]).

-define(AUTH_PREFERENCE, [
			  "CRAM-MD5",
			  "LOGIN",
			  "PLAIN"
			 ]).

-define(TIMEOUT, 1200000).

-compile(export_all).

-include("metis.hrl").

-record(connstate, {socket,         %% connection socket
		    extensions,     %% extensions from the host it connected to
		    host,           %% host (mailserver for the domain the email is going to) it is connected to
		    dmid,           %% domain ID from domain controller record
		    max_conn_time=60*1000,   %% connection time window, default 1 minute 
		    max_msg=100,             %% max number of messages before closing the connection
		    msg_counter=0,
		    queue_dir  = "priv/queue"
		   }).

start_monitor(DMID, Options) ->
    spawn_monitor(?MODULE, init, [DMID, Options]).

init(DMID, Options) ->
    ?info("~p, init started, options:~p",[self(), Options]),
    State = #connstate{dmid=DMID},
    NewOptions = lists:ukeymerge(1, lists:sort(Options), lists:sort(?DEFAULT_OPTIONS)),
    case check_options(NewOptions) of
	ok -> 
	    MaxConnTime = ?PGV(max_conn_time, NewOptions)*1000,   %% max_conn_time is given in seconds in the config file
	    MaxMsg = ?PGV(max_msg, NewOptions),
	    QueueDir = ?PGV(queue_dir, NewOptions),
	    RelayDomain = ?PGV(relay, NewOptions),
	    ?debug("~p, opening connection to relay domain:~p", [self(), RelayDomain]),

	    LookupResult = smtp_util:mxlookup(RelayDomain),
	    %% filter out nasty responses
	    MXRecords= [{Pref, MX} || {Pref, MX} <- LookupResult, MX=/="0.0.0.0", MX=/="localhost"],

	    ?debug("~p, MX records for ~p are ~p", [self(), RelayDomain, MXRecords]),
	    Hosts = case MXRecords of
			[] ->
			    [{0, RelayDomain}]; % maybe we're supposed to relay to a host directly
			_ ->
			    MXRecords
		    end,
	    case try_smtp_sessions(Hosts, NewOptions, []) of
		{ok, Socket, Extensions, Host} ->
		    NewState=State#connstate{socket        = Socket, 
					     extensions    = Extensions, 
					     host          = Host,
					     max_conn_time = MaxConnTime,
					     max_msg       = MaxMsg,
					     queue_dir     = QueueDir
					    },

		    ?debug("conn mngr ~p opened conn., dmid:~p", [self(), DMID]),

		    ?info("~p, Connected", [self()]),
		    {ok, TimerRef}=timer:send_interval(MaxConnTime, {timer, close_conn}),
		    ?debug("~p, Timer started", [self()]),

		    ?debug("~p, Entering receive loop", [self()]),
		    loop(NewState);

		E={error, FailureType, Host, Msg} ->
		    %% should send a msg to dmcontrol and dmcontrol should put the domain on hold and periodically retry connecting
		    ?info("~p, ~p",[self(), E]),
		    exit(E)
	    end;
	{error, Reason} -> 
	    exit(Reason)
    end.


loop(State) ->
    receive
	{queue_mngr, send, QID}->
	    case mnesia:transaction(fun()->mnesia:read(qc, QID) end) of 
		{atomic, [QRecord]}->
		    ?info("~p, sending email to:~p", [self(), QRecord#qc.receiver]),
		    TryCount=QRecord#qc.try_count,
		    {atomic, ok}=mnesia:transaction(fun()->mnesia:write(QRecord#qc{try_count=TryCount+1}) end),
		    
		    #connstate{dmid=DMID, host=Host, socket=Socket, extensions=Extensions}=State,
		    From=QRecord#qc.from,
		    To=QRecord#qc.receiver,
		    
		    NewState=
			try try_sending_it({From, To, QID, DMID}, Socket, Extensions, State) of
			    _ ->
				queue_mngr ! {qsender, sent, QID, DMID, State#connstate.host},
				NewCounter=State#connstate.msg_counter+1,
				MaxMsg=State#connstate.max_msg,
				case NewCounter>=MaxMsg of
				    true ->    %% cannot send more on this connection
					quit(State#connstate.socket),
					?debug("~p, exiting, reached max msg count", [self()]),
					exit(reached_max_msg);
				    false ->
					%% return with the new State
					State#connstate{msg_counter=NewCounter}
				end
			catch
			    throw:{permanent_failure, Message} ->
				reset(Socket),
				queue_mngr ! {qsender, error, permanent_failure, QID, DMID, Host, Message},
				State;
			    throw:{internal_error, Message} ->
				reset(Socket),
				queue_mngr ! {qsender, error, internal_error, QID, DMID, Host, Message},
				State;
			    throw:{FailureType, Message} ->
				reset(Socket),
				queue_mngr ! {qsender, error, FailureType, QID, DMID, Host, Message},
				State
			end,
		    queue_mngr ! {self(), conn_mngr, State#connstate.dmid, idle},
		    loop(NewState);
		{atomic, []}->
		    ?debug("~p missing from queue", [QID]),
		    loop(State);
		_Other  ->
		    ?debug("Unexpected result of reading ~p from queue: ~p", [QID, _Other]),
		    loop(State)
	    end;

	{timer, close_conn}->
	    quit(State#connstate.socket),
	    ?debug("~p, exiting, reached max connection time", [self()]),
	    exit(reached_max_conn_time);

	Any ->
	    ?debug("~p, UNEXPECTED MSG:~p", [self(), Any]),
	    loop(State)

    after 1000 ->
	    queue_mngr ! {self(), conn_mngr, State#connstate.dmid, idle},
	    loop(State)
    end.





try_smtp_sessions([{Distance, Host} | Tail], Options, RetryList) ->
    Retries = proplists:get_value(retries, Options),
    try start_smtp_session(Host, Options) of
	{ok, Socket, Extension}->
	    {ok, Socket, Extension, Host}
    catch
	throw:{permanent_failure, Message} ->
	    %% permanent failure means no retries, and don't even continue with other hosts
	    {error, permanent_failure, Host, Message};
	  throw:{internal_error, Message}->
	    {error, internal_error, Host, Message};
	  throw:{FailureType, Message} ->
	    case proplists:get_value(Host, RetryList) of
		RetryCount when is_integer(RetryCount), RetryCount >= Retries ->
		    %% out of chances
		    ?debug("~p, retries for ~p exceeded (~p of ~p)", [self(), Host, RetryCount, Retries]),
		    NewHosts = Tail,
		    NewRetryList = lists:keydelete(Host, 1, RetryList);
		RetryCount when is_integer(RetryCount) ->
		    ?debug("~p, scheduling ~p for retry (~p of ~p)", [self(), Host, RetryCount, Retries]),
		    NewHosts = Tail ++ [{Distance, Host}],
		    NewRetryList = lists:keydelete(Host, 1, RetryList) ++ [{Host, RetryCount + 1}];
		_ ->
		    %% otherwise...
		    ?debug("~p, scheduling ~p for retry (~p of ~p)", [self(), Host, 1, Retries]),
		    NewHosts = Tail ++ [{Distance, Host}],
		    NewRetryList = lists:keydelete(Host, 1, RetryList) ++ [{Host, 1}]
	    end,
	    case NewHosts of
		[] ->
		    {error, FailureType, Host, Message};
		NH ->
		    try_smtp_sessions(NH, Options, NewRetryList)
	    end
    end.

start_smtp_session(Host, Options) ->
    {ok, Socket, Host, Banner} = connect(Host, Options),
    ?debug("~p, connected to ~p; banner was ~p", [self(), Host, Banner]),
    {ok, Extensions} = try_EHLO(Socket, Options),
    %%?debug("Extensions are ~p", [Extensions]),
    {Socket2, Extensions2} = try_STARTTLS(Socket, Options, Extensions),
    %%?debug("Extensions are ~p", [Extensions2]),
    Authed = try_AUTH(Socket2, Options, proplists:get_value(<<"AUTH">>, Extensions2)),
    %%?debug("Authentication status is ~p", [Authed]),
    {ok, Socket2, Extensions2}.



try_sending_it({From, To, QID, DMID}, Socket, Extensions, State) ->
    try_MAIL_FROM(From, Socket, Extensions),
    try_RCPT_TO(To, Socket, Extensions),
    try_DATA(QID, Socket, Extensions, State#connstate.queue_dir).

try_MAIL_FROM("<" ++ _ = From, Socket, _Extensions) ->
    %% TODO do we need to bother with SIZE?
    socket:send(Socket, "MAIL FROM: "++From++"\r\n"),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"250", _Rest/binary>>} ->
	    true;
	{ok, <<"4", _Rest/binary>> = Msg} ->
	    quit(Socket),
	    throw({temporary_failure, Msg});
	{ok, Msg} ->
	    ?debug("~p, Mail FROM rejected: ~p", [self(), Msg]),
	    quit(Socket),
	    throw({permanent_failure, Msg})
    end;
try_MAIL_FROM(From, Socket, Extensions) ->
    %% someone was bad and didn't put in the angle brackets
    try_MAIL_FROM("<"++From++">", Socket, Extensions).

try_RCPT_TO("<" ++ _ = To, Socket, Extensions) ->
    socket:send(Socket, "RCPT TO: "++To++"\r\n"),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"250", _Rest/binary>>} ->
	    true;
	{ok, <<"251", _Rest/binary>>} ->
	    true;
	{ok, <<"4", _Rest/binary>> = Msg} ->
	    throw({temporary_failure, Msg});
	{ok, Msg} ->
	    throw({permanent_failure, Msg})
    end;
try_RCPT_TO(To, Socket, Extensions) ->
    %% someone was bad and didn't put in the angle brackets
    try_RCPT_TO("<"++To++">", Socket, Extensions).

try_DATA(QID, Socket, _Extensions, QueueDir) ->
    [_,_,_,_ | FN]=QID,
    FileName=QueueDir++"/qdf"++QID,
    Body=
	case file:read_file(FileName) of
	    {ok, Data}->
		binary_to_list(Data);
	    {error, _}->
		throw({internal_error, lists:flatten(io_lib:format("Missing data file for queue id:~p",[QID]))})
	end,
    socket:send(Socket, "DATA\r\n"),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"354", _Rest/binary>>} ->
	    socket:send(Socket, Body++"\r\n.\r\n"),
	    case read_possible_multiline_reply(Socket) of
		{ok, <<"250", _Rest2/binary>>} ->
		    true;
		{ok, <<"4", _Rest2/binary>> = Msg} ->
		    throw({temporary_failure, Msg});
		{ok, Msg} ->
		    throw({permanent_failure, Msg})
	    end;
	{ok, <<"4", _Rest/binary>> = Msg} ->
	    throw({temporary_failure, Msg});
	{ok, Msg} ->
	    throw({permanent_failure, Msg})
    end.

try_AUTH(_Socket, Options, []) ->
    case proplists:get_value(auth, Options) of
	always ->
	    erlang:error(no_auth);
	_ ->
	    false
    end;
try_AUTH(_Socket, Options, undefined) ->
    case proplists:get_value(auth, Options) of
	always ->
	    erlang:error(no_auth);
	_ ->
	    false
    end;
try_AUTH(Socket, Options, AuthTypes) ->
    case proplists:is_defined(username, Options) and
	proplists:is_defined(password, Options) and
	(proplists:get_value(auth, Options) =/= never) of
	false ->
	    case proplists:get_value(auth, Options) of
		always ->
		    erlang:error(no_auth);
		_ ->
		    false
	    end;
	true ->
	    Username = proplists:get_value(username, Options),
	    Password = proplists:get_value(password, Options),
	    ?debug("Auth types: ~p", [AuthTypes]),
	    Types = re:split(AuthTypes, " ", [{return, list}, trim]),
	    case do_AUTH(Socket, Username, Password, Types) of
		false ->
		    case proplists:get_value(auth, Options) of
			always ->
			    erlang:error(auth_failed);
			_ ->
			    false
		    end;
		true ->
		    true
	    end
    end.

do_AUTH(Socket, Username, Password, Types) ->
    FixedTypes = [string:to_upper(X) || X <- Types],
    ?debug("Fixed types: ~p", [FixedTypes]),
    AllowedTypes = [X  || X <- ?AUTH_PREFERENCE, lists:member(X, FixedTypes)],
    ?debug("available authentication types, in order of preference: ~p", [AllowedTypes]),
    do_AUTH_each(Socket, Username, Password, AllowedTypes).

do_AUTH_each(_Socket, _Username, _Password, []) ->
    false;
do_AUTH_each(Socket, Username, Password, ["CRAM-MD5" | Tail]) ->
    socket:send(Socket, "AUTH CRAM-MD5\r\n"),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"334 ", Rest/binary>>} ->
	    Seed64 = binstr:strip(binstr:strip(Rest, right, $\n), right, $\r),
	    Seed = base64:decode_to_string(Seed64),
	    Digest = smtp_util:compute_cram_digest(Password, Seed),
	    String = base64:encode(list_to_binary([Username, " ", Digest])),
	    socket:send(Socket, [String, "\r\n"]),
	    case read_possible_multiline_reply(Socket) of
		{ok, <<"235", _Rest/binary>>} ->
		    ?debug("authentication accepted", []),
		    true;
		{ok, Msg} ->
		    ?debug("authentication rejected: ~p", [Msg]),
		    do_AUTH_each(Socket, Username, Password, Tail)
	    end;
	{ok, Something} ->
	    ?debug("got ~p", [Something]),
	    do_AUTH_each(Socket, Username, Password, Tail)
    end;
do_AUTH_each(Socket, Username, Password, ["LOGIN" | Tail]) ->
    socket:send(Socket, "AUTH LOGIN\r\n"),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"334 VXNlcm5hbWU6\r\n">>} ->
	    ?debug("username prompt",[]),
	    U = base64:encode(Username),
	    socket:send(Socket, [U,"\r\n"]),
	    case read_possible_multiline_reply(Socket) of
		{ok, <<"334 UGFzc3dvcmQ6\r\n">>} ->
		    ?debug("password prompt",[]),
		    P = base64:encode(Password),
		    socket:send(Socket, [P,"\r\n"]),
		    case read_possible_multiline_reply(Socket) of
			{ok, <<"235 ", _Rest/binary>>} ->
			    ?debug("authentication accepted",[]),
			    true;
			{ok, Msg} ->
			    ?debug("password rejected: ~p", [Msg]),
			    do_AUTH_each(Socket, Username, Password, Tail)
		    end;
		{ok, Msg2} ->
		    ?debug("username rejected: ~p", [Msg2]),
		    do_AUTH_each(Socket, Username, Password, Tail)
	    end;
	{ok, Something} ->
	    ?debug("got ~p", [Something]),
	    do_AUTH_each(Socket, Username, Password, Tail)
    end;
do_AUTH_each(Socket, Username, Password, ["PLAIN" | Tail]) ->
    AuthString = base64:encode("\0"++Username++"\0"++Password),
    socket:send(Socket, ["AUTH PLAIN ", AuthString, "\r\n"]),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"235", _Rest/binary>>} ->
	    ?debug("authentication accepted",[]),
	    true;
	Else ->
	    %% TODO do we need to bother trying the multi-step PLAIN?
	    ?debug("authentication rejected",[]),
	    ?debug("~p", [Else]),
	    do_AUTH_each(Socket, Username, Password, Tail)
    end;
do_AUTH_each(Socket, Username, Password, [Type | Tail]) ->
    ?debug("unsupported AUTH type ~p", [Type]),
    do_AUTH_each(Socket, Username, Password, Tail).

try_EHLO(Socket, Options) ->
    ok = socket:send(Socket, "EHLO "++proplists:get_value(hostname, Options)++"\r\n"),
    {ok, Reply} = read_possible_multiline_reply(Socket),
    [_ | Reply2] = re:split(Reply, "\r\n", [{return, binary}, trim]),
    Extensions = [
		  begin
		      Body = binstr:substr(Entry, 5),
		      case re:split(Body, " ",  [{return, binary}, trim, {parts, 2}]) of
			  [Verb, Parameters] ->
			      {binstr:to_upper(Verb), Parameters};
			  [Body] ->
			      case binstr:strchr(Body, $=) of
				  0 ->
				      {binstr:to_upper(Body), true};
				  _ ->
				      ?debug("discarding option ~p", [Body]),
				      []
			      end
		      end
		  end  || Entry <- Reply2],
    {ok, Extensions}.

						% check if we should try to do TLS
try_STARTTLS(Socket, Options, Extensions) ->
    case {proplists:get_value(tls, Options),
	  proplists:get_value(<<"STARTTLS">>, Extensions)} of
	{Atom, true} when Atom =:= always; Atom =:= if_available ->
	    ?debug("Starting TLS", []),
	    case {do_STARTTLS(Socket, Options), Atom} of
		{false, always} ->
		    ?debug("TLS failed",[]),
		    erlang:exit(no_tls);
		{false, if_available} ->
		    ?debug("TLS failed",[]),
		    {Socket, Extensions};
		{{S, E}, _} ->
		    ?debug("TLS started",[]),
		    {S, E}
	    end;
	{always, _} ->
	    erlang:exit(no_tls);
	_ ->
	    {Socket, Extensions}
    end.

%% attempt to upgrade socket to TLS
do_STARTTLS(Socket, Options) ->
    socket:send(Socket, "STARTTLS\r\n"),
    case read_possible_multiline_reply(Socket) of
	{ok, <<"220", _Rest/binary>>} ->
	    crypto:start(),
	    application:start(public_key),
	    application:start(ssl),
	    case socket:to_ssl_client(Socket, [], 5000) of
		{ok, NewSocket} ->
						%NewSocket;
		    {ok, Extensions} = try_EHLO(NewSocket, Options),
		    {NewSocket, Extensions};
		Else ->
		    ?debug("~p", [Else]),
		    false
	    end;
	{ok, <<"4", _Rest/binary>> = Msg} ->
	    quit(Socket),
	    throw({temporary_failure, Msg});
	{ok, Msg} ->
	    quit(Socket),
	    throw({permanent_failure, Msg})
    end.

%% try connecting to a host
connect(Host, Options) ->
    SockOpts = [binary, {packet, line}, {keepalive, true}, {active, false}],
    Proto = case proplists:get_value(ssl, Options) of
		true ->
		    crypto:start(),
		    application:start(public_key),
		    application:start(ssl),
		    ssl;
		false ->
		    tcp
	    end,
    Port = case proplists:get_value(port, Options) of
	       undefined when Proto =:= ssl ->
		   465;
	       undefined when Proto =:= tcp ->
		   25;
	       OPort when is_integer(OPort) ->
		   OPort
	   end,
    case socket:connect(Proto, Host, Port, SockOpts, 5000) of
	{ok, Socket} ->
	    case read_possible_multiline_reply(Socket) of
		{ok, <<"220", Banner/binary>>} ->
		    {ok, Socket, Host, Banner};
		{ok, <<"4", _Rest/binary>> = Msg} ->
		    quit(Socket),
		    throw({temporary_failure, Msg});
		{ok, Msg} ->
		    quit(Socket),
		    throw({permanent_failure, Msg})
	    end;
	{error, Reason} ->
	    throw({network_failure, {error, Reason}})
    end.

%% read a multiline reply (eg. EHLO reply)
read_possible_multiline_reply(Socket) ->
    case socket:recv(Socket, 0, ?TIMEOUT) of
	{ok, Packet} ->
	    case binstr:substr(Packet, 4, 1) of
		<<"-">> ->
		    Code = binstr:substr(Packet, 1, 3),
		    read_multiline_reply(Socket, Code, [Packet]);
		<<" ">> ->
		    {ok, Packet}
	    end;
	Error ->
	    throw({network_failure, Error})
    end.

read_multiline_reply(Socket, Code, Acc) ->
    case socket:recv(Socket, 0, ?TIMEOUT) of
	{ok, Packet} ->
	    case {binstr:substr(Packet, 1, 3), binstr:substr(Packet, 4, 1)} of
		{Code, <<" ">>} ->
		    {ok, list_to_binary(lists:reverse([Packet | Acc]))};
		{Code, <<"-">>} ->
		    read_multiline_reply(Socket, Code, [Packet | Acc]);
		_ ->
		    quit(Socket),
		    throw({unexpected_response, lists:reverse([Packet | Acc])})
	    end;
	Error ->
	    throw({network_failure, Error})
    end.

quit(Socket) ->
    socket:send(Socket, "QUIT\r\n"),
    socket:close(Socket),
    ok.

reset(Socket) ->
    socket:send(Socket, "RSET\r\n"),
    ok.


						% TODO - more checking
check_options(Options) ->
    case proplists:get_value(relay, Options) of
	undefined ->
	    {error, no_relay};
	_ ->
	    case proplists:get_value(auth, Options) of
		Atom when Atom =:= always ->
		    case proplists:is_defined(username, Options) and
			proplists:is_defined(password, Options) of
			false ->
			    {error, no_credentials};
			true ->
			    ok
		    end;
		_ ->
		    ok
	    end
    end.
