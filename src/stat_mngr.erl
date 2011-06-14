%%%-------------------------------------------------------------------
%%% File    : stat_mngr.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : stat manager implemented as an event manager, event handlers keep track of different stats and save 
%%%       into riak
%%% Created : 26 Nov 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------
%% @doc stat manager implemented as an event manager, event handlers keep track of different stats and save
%% into riak

-module(stat_mngr).

-behaviour(gen_event).

-include("metis.hrl").

-export([start_link/0, add_handler/1, delete_handler/1, notify/1, call/2, which_handlers/0, timer1min/0, timer5min/0]).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    Result=gen_event:start_link({local, ?MODULE}),
    timer:apply_interval(5*60*1000, ?MODULE, timer5min, []),
    timer:apply_interval(60*1000, ?MODULE, timer1min, []),
    Handlers=?CONF(node(), event_handlers),
    ?info("Starting event handlers:~p", [Handlers]),
    lists:foreach(fun(H)->add_handler(H) end, Handlers),
    Result.

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Handler) ->
    gen_event:add_handler(?MODULE, Handler, []).

delete_handler(Handler)->
    gen_event:delete_handler(?MODULE, Handler, []).

notify(Event)->
    gen_event:notify(?MODULE, Event).

call(Handler, Request)->
    gen_event:call(?MODULE, Handler, Request).


which_handlers()->
    gen_event:which_handlers(?MODULE).




timer5min()->
    {Date,{Hr,Min,Sec}}=erlang:localtime(),
    Key=calendar:datetime_to_gregorian_seconds({Date, {Hr, trunc(Min/5)*5, 0}}),
    gen_event:notify(?MODULE, {timer5min, Key}).

timer1min()->
    {Date,{Hr,Min,Sec}}=erlang:localtime(),
    Key=calendar:datetime_to_gregorian_seconds({Date, {Hr, Min, 0}}),
    gen_event:notify(?MODULE, {timer1min, Key}).
