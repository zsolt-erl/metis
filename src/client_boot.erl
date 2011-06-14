%%% File    : client_boot.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Created :  9 Feb 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

%% @doc this is what a client node calls to start up

-module(client_boot).

-export([start/1]).

%% @doc client node start up
-spec(start/1 :: ([MngrNode :: atom()]) -> ok | {error, nostart}).
start([MngrNode])->
    error_logger:logfile({open, "priv/logs/error_logger/"++atom_to_list(node())}),
    io:format("~p node started~n", [node()]),
    {client_mngr, MngrNode} ! {node(), self(), req_conf},
    receive
    	{client_mngr, MngrPid, clientargs, ClientArgs} ->
    	    io:format("RECEIVED CLIENTARGS:~p~nFROM PID:~p~nSTARTING SUP~n", [ClientArgs,MngrPid]),

	    inets:start(),
	    mnesia:start(),
	    application:start(log4erl),
	    log4erl:conf('priv/log4erl.conf'),
	    application:start(os_mon),

    	    client_sup:start_unlink([{mngrpid, MngrPid}|ClientArgs]),
	    io:format("Queue Manager is ready~n")
    after 1000->
    	    io:format("Did not receive ClientArgs, not starting queue manager"),
	    {error, nostart}
    end.

