%%% File    : bigtimer.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : a timer module using erlang:send_after to generate timeouts, interface is same as in the timer module
%%%   (timer.erl in stdlib does not work in OTP R14B02 if Delay is bigger then 2^31/1000 in ms which is about 35min,
%%%    erlang:send_after works up to about 19.88 days (Delay is 32bit int())
%%% Created : 18 May 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(bigtimer).

%% API exports
-export([send_after/2, send_after/3, apply_after/4, send_interval/2, send_interval/3, apply_interval/4]).

-export([waiter/0, waiter_interval/1]).


send_after(Delay, Msg)->
    send_after(Delay, self(), Msg).

send_after(Delay, Dest, Msg)->
    apply_after(Delay, erlang, send, [Dest, Msg]). 

apply_after(Delay, M, F, A)->
    WaiterPid=spawn(?MODULE, waiter, []),
    erlang:send_after(Delay, WaiterPid, {apply, M, F, A}),
    WaiterPid.

waiter()->
    receive
	{apply, M, F, A} -> apply(M, F, A);
	Other            -> exit({unexpected_msg, Other})
    end.



send_interval(Delay, Msg)->
    send_interval(Delay, self(), Msg).

send_interval(Delay, Dest, Msg)->
    apply_interval(Delay, erlang, send, [Dest, Msg]). 

apply_interval(Delay, M, F, A)->
    WaiterPid=spawn(?MODULE, waiter_interval, [Delay]),
    erlang:send_after(Delay, WaiterPid, {apply, M, F, A}),
    {ok, WaiterPid}.

waiter_interval(Delay)->
    receive
	{apply, M, F, A} -> 
	    apply(M, F, A),
	    erlang:send_after(Delay, self(), {apply, M, F, A}),
	    waiter_interval(Delay);
	Other -> 
	    exit({unexpected_msg, Other})
    end.
