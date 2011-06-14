-module(metis_sup).

-vsn(0.1).

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
    ?info("Metis Supervisor started", []),

    SetUID={setuid, {setuid, start_link, []}, permanent, 10000, worker, [setuid]},

    ServerName   = ?CONF(main, server),
    ServerParams = ?CONF_SECTION(ServerName),
    io:format("server params:~p~n",[ServerParams]),
    
    ServerHost   = ?PGV(local_fqdn, ServerParams),
    ServerPort   = ?PGV(port, ServerParams),
    ServerIP     = ?PGV(ip, ServerParams),
    ServerRunAs  = ?PGV(run_as, ServerParams),

    io:format("local fqdn:~p~n",[ServerHost]),

    Server={
      smtp_server,
      {gen_smtp_server, start_link, 
       [
	{local, smtp_server},     %% servername
	metis_smtp_server,        %% callback module
	%% server options
	[ [{domain, ServerHost}, {port, ServerPort}, {address, ServerIP}, {run_as, ServerRunAs}, 
	   {sessionoptions, [{allow_bare_newlines, fix}, {callbackoptions, [{instant_relay, ["joe@example.org"]}]}]}] ]
       ]
      },
      permanent, 10000, worker, [gen_smtp_server, gen_smtp_server_session, metis_smtp_server]
     },

    StatMngr={stat_manager, {stat_mngr, start_link, []}, permanent, 10000, worker, dynamic},
    ClientMngr={client_manager, {client_mngr, start_link, []}, permanent, 10000, worker, [client_mngr]},

    {ok, {{one_for_one, 3, 10}, [SetUID, Server, StatMngr, ClientMngr]}}.
