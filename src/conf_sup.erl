-module(conf_sup).
-vsn(0.01).

-behaviour(supervisor).

-export( [start/0, start_inshell/0, start_link/0, init/1] ).

-include("metis.hrl").

start()->
    spawn(fun()->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg=[])
	  end).

start_inshell()->
    {ok, Pid}=supervisor:start_link(?MODULE, ?MODULE, _Arg=[]),
    unlink(Pid).

start_link()->
    supervisor:start_link({local,?MODULE}, ?MODULE, _Args=[]).

init(_Args)->
    {vsn, [Version]}=lists:keyfind(vsn, 1, ?MODULE:module_info(attributes)),
    ?info("Config Supervisor started. version:~p", [Version]),
    {ok, 
     {{one_for_one,3,10},
      [
       {
	 config_server,
	 {conf, start_link, ['priv/mta.conf'] },
	 permanent,
	 10000,
	 worker,
	 [conf]
	}
      ]
     }
    }.
