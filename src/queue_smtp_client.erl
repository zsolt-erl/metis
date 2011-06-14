%%% File    : queue_smtp_client.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% @doc Description : this is used by the queue manager to send emails from queue when no connection manager is used for the destination domain

%%% Created : 31 Mar 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

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


-module(queue_smtp_client).

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-else.
-export([send/2, send_it/2]).
-endif.

-compile(export_all).

-include("metis.hrl").



%% @spec( send/2 :: (Email :: {From:: string(), To::string(), QID::string()}, Options :: list()) -> {'ok', pid()} | {'error', any()}).
send(Email={_From, _To, QID, DMID}, Options) ->
    ?info("sending:~p", [QID]),
    ?debug("send options:~p",[lists:flatten(io_lib:format("~p",[Options]))]),

    NewOptions = lists:ukeymerge(1, lists:sort(Options), lists:sort(?DEFAULT_OPTIONS)),
    case check_options(NewOptions) of
	ok              -> (?MODULE):send_it(Email, NewOptions);
	{error, Reason} -> {error, Reason}
    end.

%% @spec( send_it/2 :: (Email :: {From::string(), To::string(), QID::string()}, Options :: list()) -> 'ok').
send_it(Email, Options) ->
    RelayDomain = proplists:get_value(relay, Options),
    
    LookupResult = smtp_util:mxlookup(RelayDomain),
    %% filter out nasty responses
    MXRecords= [{Pref, MX} || {Pref, MX} <- LookupResult, MX=/="0.0.0.0", MX=/="localhost"],
    
    ?info("MX records for ~p are ~p", [RelayDomain, MXRecords]),
    Hosts = case MXRecords of
		[] ->
		    [{0, RelayDomain}]; % maybe we're supposed to relay to a host directly
		_ ->
		    MXRecords
	    end,
    try_smtp_sessions(Hosts, Email, Options, []).

try_smtp_sessions([{Distance, Host} | Tail], Email={_From, _To, QID, DMID}, Options, RetryList) ->
    Retries = proplists:get_value(retries, Options),
    try do_smtp_session(Host, Email, Options) of
	ok->
	    queue_mngr ! {qsender, sent, QID, DMID, Host}
    catch
	throw:{permanent_failure, Message} ->
	    %% permanent failure means no retries, and don't even continue with other hosts
	    queue_mngr ! {qsender, error, permanent_failure, QID, DMID, Host, Message};
	throw:{internal_error, Message}->
	    queue_mngr ! {qsender, error, internal_error, QID, DMID, Host, Message};
	throw:{FailureType, Message} ->
	    case proplists:get_value(Host, RetryList) of
		RetryCount when is_integer(RetryCount), RetryCount >= Retries ->
		    %% out of chances
		    ?debug("retries for ~p exceeded (~p of ~p)", [Host, RetryCount, Retries]),
		    NewHosts = Tail,
		    NewRetryList = lists:keydelete(Host, 1, RetryList);
		RetryCount when is_integer(RetryCount) ->
		    ?debug("scheduling ~p for retry (~p of ~p)", [Host, RetryCount, Retries]),
		    NewHosts = Tail ++ [{Distance, Host}],
		    NewRetryList = lists:keydelete(Host, 1, RetryList) ++ [{Host, RetryCount + 1}];
		_ ->
		    %% otherwise...
		    ?debug("scheduling ~p for retry (~p of ~p)", [Host, 1, Retries]),
		    NewHosts = Tail ++ [{Distance, Host}],
		    NewRetryList = lists:keydelete(Host, 1, RetryList) ++ [{Host, 1}]
	    end,
	    case NewHosts of
		[] ->
		    queue_mngr ! {qsender, error, FailureType, QID, DMID, Host, Message};
		_ ->
		    try_smtp_sessions(NewHosts, Email, Options, NewRetryList)
	    end
    end.

do_smtp_session(Host, Email, Options) ->
    {ok, Socket, Host, Banner} = connect(Host, Options),
    ?info("connected to ~p; banner was ~s", [Host, Banner]),
    {ok, Extensions} = try_EHLO(Socket, Options),
    %%?debug("Extensions are ~p", [Extensions]),
    {Socket2, Extensions2} = try_STARTTLS(Socket, Options,
					  Extensions),
    %%?debug("Extensions are ~p", [Extensions2]),
    Authed = try_AUTH(Socket2, Options, proplists:get_value(<<"AUTH">>, Extensions2)),
    %%?debug("Authentication status is ~p", [Authed]),
    try_sending_it(Email, Socket2, Extensions2, Options),
    ?info("Mail sending successful", []),
    quit(Socket2),
    ok.

try_sending_it({From, To, QID, DMID}, Socket, Extensions, Options) ->
    try_MAIL_FROM(From, Socket, Extensions),
    try_RCPT_TO(To, Socket, Extensions),
    try_DATA(QID, Socket, Extensions, ?PGV(queue_dir, Options)).

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
	    ?debug("Mail FROM rejected: ~p", [Msg]),
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
		throw({internal_error, lists:flatten(io_lib:format("Missing data file for queue id:~s",[QID]))})
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
		    ?debug("authentication rejected: ~s", [Msg]),
		    do_AUTH_each(Socket, Username, Password, Tail)
	    end;
	{ok, Something} ->
	    ?debug("got ~s", [Something]),
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
			    ?debug("password rejected: ~s", [Msg]),
			    do_AUTH_each(Socket, Username, Password, Tail)
		    end;
		{ok, Msg2} ->
		    ?debug("username rejected: ~s", [Msg2]),
		    do_AUTH_each(Socket, Username, Password, Tail)
	    end;
	{ok, Something} ->
	    ?debug("got ~s", [Something]),
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
    ?debug("unsupported AUTH type ~s", [Type]),
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
    SockOpts0 = [binary, {packet, line}, {keepalive, true}, {active, false}],

    SockOpts = case proplists:lookup(ip, Options) of 
		   none -> SockOpts0;
		   Tuple -> SockOpts0++[Tuple]
	       end,
		   
    ?debug("SOCKOPTS:~p",[lists:flatten(io_lib:format("~p",[SockOpts]))]),

    %% ? here SockOpts has to have {ip, xxx} of the interface we want to send on

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

-ifdef(TEST).

session_start_test_() ->
    {foreach,
     local,
     fun() ->
	     {ok, ListenSock} = socket:listen(tcp, 9876),
	     {ListenSock}
     end,
     fun({ListenSock}) ->
	     socket:close(ListenSock)
     end,
     [fun({ListenSock}) ->
	      {"simple session initiation",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"retry on crashed EHLO twice if requested",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {retries, 2}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:close(X),
		       {ok, Y} = socket:accept(ListenSock, 1000),
		       socket:send(Y, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:close(Y),
		       {ok, Z} = socket:accept(ListenSock, 1000),
		       socket:send(Z, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Z, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"retry on crashed EHLO",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       unlink(Pid),
		       Monitor = erlang:monitor(process, Pid),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:close(X),
		       {ok, Y} = socket:accept(ListenSock, 1000),
		       socket:send(Y, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:close(Y),
		       ?assertEqual({error, timeout}, socket:accept(ListenSock, 1000)),
		       receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, retries_exceeded, _}, Error) end,
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"abort on 554 greeting",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       unlink(Pid),
		       Monitor = erlang:monitor(process, Pid),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "554 get lost, kid\r\n"),
		       ?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
		       receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, no_more_hosts, _}, Error) end,
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"retry on 421 greeting",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "421 can't you see I'm busy?\r\n"),
		       ?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
		       {ok, Y} = socket:accept(ListenSock, 1000),
		       socket:send(Y, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"retry on messed up EHLO response",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       unlink(Pid),
		       Monitor = erlang:monitor(process, Pid),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250-server.example.com EHLO\r\n250-AUTH LOGIN PLAIN\r\n421 too busy\r\n"),
		       ?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),

		       {ok, Y} = socket:accept(ListenSock, 1000),
		       socket:send(Y, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:send(Y, "250-server.example.com EHLO\r\n250-AUTH LOGIN PLAIN\r\n421 too busy\r\n"),
		       ?assertMatch({ok, "QUIT\r\n"}, socket:recv(Y, 0, 1000)),
		       receive {'DOWN', Monitor, _, _, Error} -> ?assertMatch({error, retries_exceeded, _}, Error) end,
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"a valid complete transaction without TLS advertised should succeed",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250 hostname\r\n"),
		       ?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250 ok\r\n"),
		       ?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250 ok\r\n"),
		       ?assertMatch({ok, "DATA\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "354 ok\r\n"),
		       ?assertMatch({ok, "hello world\r\n"}, socket:recv(X, 0, 1000)),
		       ?assertMatch({ok, ".\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250 ok\r\n"),
		       ?assertMatch({ok, "QUIT\r\n"}, socket:recv(X, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"a valid complete transaction with TLS advertised should succeed",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250-hostname\r\n250 STARTTLS\r\n"),
		       ?assertMatch({ok, "STARTTLS\r\n"}, socket:recv(X, 0, 1000)),
		       application:start(crypto),
		       application:start(public_key),
		       application:start(ssl),
		       socket:send(X, "220 ok\r\n"),
		       {ok, Y} = socket:to_ssl_server(X, [{certfile, "../testdata/server.crt"}, {keyfile, "../testdata/server.key"}], 5000),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:send(Y, "250-hostname\r\n250 STARTTLS\r\n"),
		       ?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:send(Y, "250 ok\r\n"),
		       ?assertMatch({ok, "RCPT TO: <foo@bar.com>\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:send(Y, "250 ok\r\n"),
		       ?assertMatch({ok, "DATA\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:send(Y, "354 ok\r\n"),
		       ?assertMatch({ok, "hello world\r\n"}, socket:recv(Y, 0, 1000)),
		       ?assertMatch({ok, ".\r\n"}, socket:recv(Y, 0, 1000)),
		       socket:send(Y, "250 ok\r\n"),
		       ?assertMatch({ok, "QUIT\r\n"}, socket:recv(Y, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"AUTH PLAIN should work",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {username, "user"}, {password, "pass"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250-hostname\r\n250 AUTH PLAIN\r\n"),
		       AuthString = binary_to_list(base64:encode("\0user\0pass")),
		       AuthPacket = "AUTH PLAIN "++AuthString++"\r\n",
		       ?assertEqual({ok, AuthPacket}, socket:recv(X, 0, 1000)),
		       socket:send(X, "235 ok\r\n"),
		       ?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"AUTH LOGIN should work",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {username, "user"}, {password, "pass"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250-hostname\r\n250 AUTH LOGIN\r\n"),
		       ?assertEqual({ok, "AUTH LOGIN\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "334 VXNlcm5hbWU6\r\n"),
		       UserString = binary_to_list(base64:encode("user")),
		       ?assertEqual({ok, UserString++"\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "334 UGFzc3dvcmQ6\r\n"),
		       PassString = binary_to_list(base64:encode("pass")),
		       ?assertEqual({ok, PassString++"\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "235 ok\r\n"),
		       ?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
		       ok
	       end
	      }
      end,
      fun({ListenSock}) ->
	      {"AUTH CRAM-MD5 should work",
	       fun() ->
		       Options = [{relay, "localhost"}, {port, 9876}, {hostname, "testing"}, {username, "user"}, {password, "pass"}],
		       {ok, _Pid} = send({"test@foo.com", ["foo@bar.com"], "hello world"}, Options),
		       {ok, X} = socket:accept(ListenSock, 1000),
		       socket:send(X, "220 Some banner\r\n"),
		       ?assertMatch({ok, "EHLO testing\r\n"}, socket:recv(X, 0, 1000)),
		       socket:send(X, "250-hostname\r\n250 AUTH CRAM-MD5\r\n"),
		       ?assertEqual({ok, "AUTH CRAM-MD5\r\n"}, socket:recv(X, 0, 1000)),
		       Seed = smtp_util:get_cram_string(smtp_util:guess_FQDN()),
		       DecodedSeed = base64:decode_to_string(Seed),
		       Digest = smtp_util:compute_cram_digest("pass", DecodedSeed),
		       String = binary_to_list(base64:encode(list_to_binary(["user ", Digest]))),
		       socket:send(X, "334 "++Seed++"\r\n"),
		       {ok, Packet} = socket:recv(X, 0, 1000),
		       CramDigest = smtp_util:trim_crlf(Packet),
		       ?assertEqual(String, CramDigest),
		       socket:send(X, "235 ok\r\n"),
		       ?assertMatch({ok, "MAIL FROM: <test@foo.com>\r\n"}, socket:recv(X, 0, 1000)),
		       ok
	       end
	      }
      end

     ]
    }.


-endif.
