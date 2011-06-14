%%% File    : hriaklogger.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Created : 23 Dec 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

%% @doc event handler for stat manager, logs events and periodically saves them to riak

%% Key :: binary(), Value :: binary(),  created with term_to_binary(),
%% key :: datetime() converted to seconds 
%% value ::  [Entry :: entry()]
%% type(entry() :: [ Module :: atom(),
%%                   DateTime :: datetime(),
%%                   EntryType :: sent | queued | deleted | softbounce | hardbounce | network_failure | Other ,
%%                   Message :: term(),
%%                   QC :: #qc{},
%%                   OtherData :: term() 
%%                 ]

-module(hriaklogger).

-behaviour(gen_event).

-include("metis.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,  handle_info/2, terminate/2, code_change/3, clear_bucket/2]).


-record(state, {riak_client}).
-record(eventstemp, {key, event}).

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
    {ok, RCPid}=riakc_pb_socket:start_link( conf:get(riak, pb_ip), conf:get(riak, pb_port) ),
    ?info("Connected to riak", []),
    riakc_pb_socket:set_options(RCPid, [queue_if_disconnected], 10000),

    case lists:member(eventstemp, mnesia:system_info(tables)) of
	true->
	    mnesia:clear_table(eventstemp);
	false ->
	    ?debug("Creating eventstemp table", []),
	    {atomic, ok}=mnesia:create_table(eventstemp, [{attributes, record_info(fields, eventstemp)}])
    end,
    ?info("eventstemp Mnesia table is ready", []),
    ?info("Init done.",[]),
    {ok, #state{riak_client=RCPid}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------

handle_event(Event, State)
  when is_record(Event, riaklogentry) ->
    %% write log entry into mnesia table (key is erlang:now())
    {atomic, ok}=mnesia:transaction(fun()->mnesia:write(#eventstemp{key=erlang:now(), event=Event}) end),
    {ok, State};

handle_event({timer5min, Key}, State=#state{riak_client=RCPid})->
    %% get data from mnesia table
    MatchHead = #eventstemp{event='$1', _='_'},
    Result = '$1',
    {atomic, Data}=mnesia:transaction(fun()->mnesia:select(eventstemp,[{MatchHead, [], [Result]}]) end),
    %% ?debug("log data:~p",[Data]),

    case Data of
	[] ->
	    ok;
	_ ->
	    %% write data to riak (key is Key)
	    Object=riakc_obj:new(<<"eventlog">>, term_to_binary(Key), term_to_binary(Data)),
	    ok = riakc_pb_socket:put(RCPid, Object)
    end,

    %% clear mnesia table
    mnesia:clear_table(eventstemp),
    {ok, State};

handle_event(Event, State) ->
    ?debug("UNKNOWN EVENT received:~p", [Event]),
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


handle_call(get_last100_events, State) ->
    Keys=lists:sort( mnesia:dirty_all_keys(eventstemp) ),
    Last100=
	case length(Keys) of
	    L1 when L1 =< 100 -> Keys;
	    L2 -> lists:nthtail(L2-100, Keys)
	end,
    LogEntries=
	lists:foldl(fun(Key, Acc)->
			    case mnesia:dirty_read(eventstemp, Key) of
				[{_, _Key, Value}] -> Acc++[Value];
				_Other  -> Acc
			    end
		    end, [], Last100),
    {ok, LogEntries, State};



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

clear_bucket(Pid, Bucket)->
    {ok, Keys}=riakc_pb_socket:list_keys(Pid, Bucket),
    lists:foreach(fun(Key)->
			  riakc_pb_socket:delete(Pid, Bucket, Key)
		  end,
		  Keys).

    
