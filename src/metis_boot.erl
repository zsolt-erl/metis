-module(metis_boot).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    error_logger:logfile({open, "priv/logs/error_logger/"++atom_to_list(node())}),
    inets:start(),
    application:start(log4erl),
    log4erl:conf("priv/log4erl.conf"),
    mnesia:start(),

    conf_sup:start_link(),
    metis_sup:start_link().

stop(_State) ->
    ok.
