-module(metis).

-include("metis.hrl").

-export([start/0, stop/0, info/0]).

start() ->
    application:start(metis).


stop() ->
    application:stop(metis).


info() ->
    io:format("~n========== NODES =====================================================================================~n"),
    io:format("SMTP server:  ~s   listening on    ~p:~p ~n", [node(), ?CONF(node(), ip), ?CONF(node(), port)]),
    io:format("SMTP clients: ~p~n", [nodes()]),
    io:format("======================================================================================================~n~n"),

    io:format("~n========== APPLICATIONS ==============================================================================~n"),
    io:format("~p~n", [application:which_applications()]),
    io:format("======================================================================================================~n~n"),

    io:format("============ PROCESSES ===============================================================================~n"),
    io:format("smtp_server:~p~n", [whereis(smtp_server)]),
    io:format("client_mngr:~p~n", [whereis(client_mngr)]),
    io:format("stat_mngr:  ~p~n", [whereis(stat_mngr)]),
    io:format("======================================================================================================~n~n"),

    io:format("============ HANDLERS ================================================================================~n"),
    io:format("~p~n", [stat_mngr:which_handlers()]),
    io:format("======================================================================================================~n~n"),

    io:format("============ STATS ===================================================================================~n"),
    io:format("~p~n", [stat_mngr:call(hsimplecount, get_all_totals)]),
    io:format("======================================================================================================~n~n"),

    ok.
