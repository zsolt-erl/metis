%%% File    : client_sup.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Created : 26 Feb 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

%% @doc runs on the client and supervises the queue manager

-module(client_sup).
-vsn(0.1).

-behaviour(supervisor).

-export( [start/1, start_unlink/1, start_link/1, init/1] ).

-include("metis.hrl").

start(ClientArgs)->
    spawn(fun()->
		  supervisor:start_link({local,?MODULE}, ?MODULE, ClientArgs)
	  end).

start_unlink(ClientArgs)->
    {ok, Pid}=supervisor:start_link({local, ?MODULE}, ?MODULE, ClientArgs),
    unlink(Pid),
    Pid.

start_link(ClientArgs)->
    supervisor:start_link({local,?MODULE}, ?MODULE, ClientArgs).

init(ClientArgs)->
    {vsn, [Version]}=lists:keyfind(vsn, 1, ?MODULE:module_info(attributes)),
    ?info("Metis Client Supervisor started on ~p. version:~p~n", [node(), Version]),
    {ok, 
     {{one_for_one,3,10},
      [
       {
	 queue_manager,
	 {queue_mngr, start_link, [ClientArgs] },
	 permanent,
	 10000,
	 worker,
	 [queue_mngr]
	}
      ]
     }
    }.
