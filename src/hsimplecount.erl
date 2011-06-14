%%%-------------------------------------------------------------------
%%% File    : hsimplecount.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Created : 26 Nov 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------

%% @doc event handler for stat manager, counts queued/sent/deleted/softbounce/hardbounce emails and
%%      keeps track of them per minute for the last hr



-module(hsimplecount).
 
-behaviour(gen_event).

-include("metis.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(counters, {
	  queued=0,
	  sent  =0,
	  deleted   =0,
	  softbounce=0,
	  hardbounce=0,
	  ptqueue
	 }).

-record(state, {
	  counters=#counters{},
	  totals=#counters{},
	  timeline=flq:new(60, {0, #counters{}})                    %% list of counters for last 60 min
	 }).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}). 

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Handler) ->
    gen_event:add_handler(?MODULE, Handler, []).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
    ?info("Init done.",[]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------

%% update counters
handle_event(#riaklogentry{type=sent}, State=#state{counters=Counters, totals=Totals}) ->
    C=Counters#counters.sent+1,
    CT=Totals#counters.sent+1,
    {ok, State#state{counters=Counters#counters{sent=C}, totals=Totals#counters{sent=CT}}};

handle_event(#riaklogentry{type=queued}, State=#state{counters=Counters, totals=Totals}) ->
    C=Counters#counters.queued+1,
    CT=Totals#counters.queued+1,
    {ok, State#state{counters=Counters#counters{queued=C}, totals=Totals#counters{queued=CT}}};

handle_event(#riaklogentry{type=deleted}, State=#state{counters=Counters, totals=Totals}) ->
    C=Counters#counters.deleted+1,
    CT=Totals#counters.deleted+1,
    {ok, State#state{counters=Counters#counters{deleted=C}, totals=Totals#counters{deleted=CT}}};

handle_event(#riaklogentry{type=temporary_failure}, State=#state{counters=Counters, totals=Totals}) ->
    C=Counters#counters.softbounce+1,
    CT=Totals#counters.softbounce+1,
    {ok, State#state{counters=Counters#counters{softbounce=C}, totals=Totals#counters{softbounce=CT}}};

handle_event(#riaklogentry{type=network_failure}, State=#state{counters=Counters, totals=Totals}) ->
    %% this is not a softbounce, it should not be handled as that

    %% C=Counters#counters.softbounce+1,
    %% CT=Totals#counters.softbounce+1,
    C=Counters#counters.softbounce,
    CT=Totals#counters.softbounce,
    {ok, State#state{counters=Counters#counters{softbounce=C}, totals=Totals#counters{softbounce=CT}}};

handle_event(#riaklogentry{type=permanent_failure}, State=#state{counters=Counters, totals=Totals}) ->
    C=Counters#counters.hardbounce+1,
    CT=Totals#counters.hardbounce+1,
    {ok, State#state{counters=Counters#counters{hardbounce=C}, totals=Totals#counters{hardbounce=CT}}};

handle_event({Other, _QRec, _Time}, State) ->
    ?debug("QSENDER EVENT received:~p", [Other]),
    {ok, State};

%% timer events
handle_event({timer1min, Key}, State=#state{counters=Counters, timeline=TL})->
    NewCounters=#counters{},
    PTQueue=client_mngr:call(get_total_queue_len),
    {_, NewTL}=flq:in({Key, Counters#counters{ptqueue=PTQueue}}, TL),
    {ok, State#state{counters=NewCounters, timeline=NewTL}};

handle_event(Event, State) ->
    ?debug("EVENT received:~p", [Event]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(get_sent, State=#state{counters=Counters}) ->
    Reply=Counters#counters.sent,
    {ok, Reply, State};

handle_call(get_queued, State=#state{counters=Counters}) ->
    Reply=Counters#counters.queued,
    {ok, Reply, State};

handle_call(get_all_pt, State=#state{counters=Counters}) ->
    PTQueue=client_mngr:call(get_total_queue_len),
    Reply=zipcounters(Counters#counters{ptqueue=PTQueue}),
    {ok, Reply, State};

handle_call(get_all_totals, State=#state{totals=Totals}) ->
    PTQueue=client_mngr:call(get_total_queue_len),
    Reply=zipcounters(Totals#counters{ptqueue=PTQueue}),
    {ok, Reply, State};

handle_call(get_tl, State=#state{timeline=TL}) ->
    Reply=flq:to_list(TL),
    {ok, Reply, State};


handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

zipcounters(Record)->
    [RecName|Data]=tuple_to_list(Record),
    FieldNames=record_info(fields, counters),
    lists:zip(FieldNames, Data).
