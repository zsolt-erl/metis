-module(profile).

-export([start/2]).

start(ProcessName, Interval)->
    fprof:trace([start, {procs, whereis(ProcessName)}]),
    io:format("Started tracing\n"),
    receive
    after 
	Interval->
	    ok
    end,
    fprof:trace([stop]),
    io:format("Stopped tracing\n"),

    io:format("Profiling\n"),
    fprof:profile(),
    io:format("Finished profiling\n"),

    io:format("Analyzing\n"),
    fprof:analyse([{dest,[]}]),
    io:format("Finished analyzing\n").
