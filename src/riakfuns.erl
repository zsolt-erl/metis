%%% File    : riakfuns.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : functions for riak mapreduce 
%%% Created :  4 Jan 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(riakfuns).

-include("metis.hrl").


-compile(export_all).

-define(Print(Var), io:format("~s = ~p~n", [??Var, Var])).

-define(P(M), io:format("!!! ~p~n",[M])).
    
ensure_string(X)->io_lib:format("~p", [X]).

get_field({FieldName}, Entry)->
    (?MRIAKLOGENTRY):get(FieldName, Entry);
get_field({qc, FieldName}, Entry) ->
    QC=(?MRIAKLOGENTRY):get(qc, Entry),
    (?MQC):get(FieldName, QC);
get_field({qc, opaque, FieldName}, Entry) ->
    QC=(?MRIAKLOGENTRY):get(qc, Entry),
    Opaque=(?MQC):get(opaque, QC),
    ?PGV(FieldName, Opaque).


%% START--FILTERED-ENTRIES------------------------------------

run_one_filter({FieldDef, Regexp}, Entry) ->
     FieldValue=get_field(FieldDef, Entry),
     re:run(ensure_string(FieldValue), Regexp) =/= nomatch.
	      

-spec run_filters/2 :: (Filters :: list(), Entry :: #riaklogentry{}) -> passed | failed .
run_filters([], _Entry) -> true;
run_filters([Filter|Rest], Entry) ->
    case run_one_filter(Filter, Entry) of
 	true  -> run_filters(Rest, Entry);
 	false -> false
    end.
     
%% @doc gets the value and filters the records
%% this needs to return a list generated from the Value
%% each Value in the eventlog bucket is a list of #riaklogentry{} records
%% so I'm just returning the filtered Value here
mapfun_get_filtered_log_entries({error, notfound}, _KD, _Arg)  -> [];
mapfun_get_filtered_log_entries({error, not_found}, _KD, _Arg) -> [];
mapfun_get_filtered_log_entries(ValueObject, _KeyData, Filters)    -> 
    LogEntryGroup=binary_to_term(riak_object:get_value(ValueObject)),
    lists:filter(fun(Entry)->run_filters(Filters, Entry) end, LogEntryGroup).


%% @doc generates a list of possible keys between StartDateTime and EndDateTime
-spec get_key_list/2 :: (StartDateTime :: tuple(), EndDateTime :: tuple()) -> KeyList :: [binary()].
get_key_list(StartDateTime, EndDateTime)->
    End=calendar:datetime_to_gregorian_seconds(EndDateTime),
    {Date,{Hr,Min,_Sec}}=StartDateTime,
    FirstKey=calendar:datetime_to_gregorian_seconds({Date, {Hr, trunc(Min/5)*5, 0}}),
    %% 300sec is 5min, keys are 5min apart
    NumberOfKeys=trunc((End-FirstKey)/300)+1,
    [ term_to_binary(FirstKey+I*300) || I <- lists:seq(0,NumberOfKeys-1)].



%% @doc gets the logentries from the eventlog bucket between StartDateTime and EndDateTime and filters them according to Filters
-type fielddef() :: {FieldName :: atom()} | {qc, FieldName :: atom()} | {qc, opaque, FieldName :: atom()} .
-spec get_filtered_log_entries/3 :: (StartDateTime :: tuple(), 
				     EndDateTime :: tuple(), 
				     Filters :: [{FieldDef :: fielddef(), Regexp :: string()}])-> [#riaklogentry{}] .
get_filtered_log_entries(StartDateTime={SD,ST}, EndDateTime={ED,ET}, Filters)
  when is_tuple(SD) and is_tuple(ST) and is_tuple(ED) and is_tuple(ET) ->
    {ok, RCPid}=riakc_pb_socket:start_link('127.0.0.1', 8087),

    Inputs=[ {<<"eventlog">>,K} || K <- get_key_list(StartDateTime, EndDateTime) ],

    Query=[ {map, {modfun, riakfuns, mapfun_get_filtered_log_entries}, Filters, true} ],
    {ok, [{0,Result}]}=riakc_pb_socket:mapred(RCPid, Inputs, Query),
    riakc_pb_socket:stop(RCPid),
    lists:keysort(4,Result);

get_filtered_log_entries(StartDate, EndDate, Filters)
  when is_tuple(StartDate) and is_tuple(EndDate) ->
    get_filtered_log_entries({StartDate, {0,0,0}}, {EndDate, {0,0,0}}, Filters).

%% END----FILTERED-ENTRIES------------------------------------

%% START--GROUPED-COUNTS--------------------------------------

%% @doc gets the value (does not do any processing)
%% this needs to return a list generated from the Value
%% the Values in the eventlog bucket are lists of #riaklogentry{} records
%% so I'm just returning the Value here
%% Returns: [Res]
%%    Res is a dictionary
%%       Key is {[{FieldDef, Value}], {ts, TimeSlot}}
%%       Value is Count  (how many log entries for that group for that timeslot)
mapfun_get_filtered_grouped_counts({error, notfound}, _KD, _Arg)  -> [];
mapfun_get_filtered_grouped_counts({error, not_found}, _KD, _Arg) -> [];
mapfun_get_filtered_grouped_counts(ValueObject, _KeyData, {Start, End, Filters, Groups})    -> 
    LogEntryGroup=binary_to_term(riak_object:get_value(ValueObject)),
    Res=lists:foldl(fun(Entry, Acc)->
			    case run_filters(Filters, Entry) of
				false  -> Acc;
				true  -> 
				    Fields=[{FieldDef, get_field(FieldDef, Entry)} || FieldDef <- Groups],
				    EntryTime=calendar:datetime_to_gregorian_seconds(get_field({time}, Entry)),
				    TimeSlot=round( (EntryTime-Start)/((End-Start)/100) ),
				    dict:update_counter({Fields,{ts, TimeSlot}}, 1, Acc)
			    end
		    end,
		    dict:new(), LogEntryGroup),
%%    ?P({result_size_map, dict:size(Res)}),
    [Res].

-spec reducefun_get_filtered_grouped_counts/2 :: (CountDicts :: [dict()], Arg :: term()) -> [dict()].
reducefun_get_filtered_grouped_counts(CountDicts, _Arg)->
    Res=lists:foldl(fun(CountDict, Acc)->
			    dict:merge(fun(_, X, Y)-> X+Y end, CountDict, Acc)
		    end, dict:new(), CountDicts),
    [Res].

   
%% @doc selects log enties between StartDateTime, and EndDateTime, filters them according to Filters
%% groups them according to Groups and returns the different groups with counts of records belonging to that group
-spec get_filtered_grouped_counts/4 :: (StartDateTime :: tuple(), 
					EndDateTime   :: tuple(),
					Filters       :: [{FieldDef :: fielddef(), Regexp :: string()}],
					Groups        :: [fielddef()]) 
				       -> [ {[{fielddef(), FieldValue :: any()}], Count :: integer()} ].
					        
get_filtered_grouped_counts(StartDateTime={SD,ST}, EndDateTime={ED,ET}, Filters, Groups)
  when is_tuple(SD) and is_tuple(ST) and is_tuple(ED) and is_tuple(ET) ->
    Start=calendar:datetime_to_gregorian_seconds(StartDateTime),
    End=calendar:datetime_to_gregorian_seconds(EndDateTime),

    {ok, RCPid}=riakc_pb_socket:start_link('127.0.0.1', 8087),

    Inputs=[ {<<"eventlog">>,K} || K <- get_key_list(StartDateTime, EndDateTime) ],
    Query=[ {map, {modfun, riakfuns, mapfun_get_filtered_grouped_counts}, {Start, End, Filters, Groups}, false},
	    {reduce, {modfun, riakfuns, reducefun_get_filtered_grouped_counts}, none, true}],
    
    {ok, [{1,[Result]}]}=riakc_pb_socket:mapred(RCPid, Inputs, Query),
    riakc_pb_socket:stop(RCPid),
    ?P(finished_riak),
    ?P({result_size, dict:size(Result)}),
    ?P(dict:to_list(Result)),
    %% this is a dict
    %%   Key   : Group :: [{FieldDef, FieldValue}]
    %%   Value : [{sum, Sum}, {tl, TimeLine}]
    %%            Sum : sum of all counts for this group
    %%            TimeLine:  orddict() Counts by TimeSlots
    ResultByGroups=
	dict:fold(fun({Group, {ts, TimeSlot}}, Count, AccIn)->
			  dict:update(Group, 
				      fun([{sum, Sum}, {tl, TimeLine}])->
					      NewSum=Sum+Count,
					      NewTimeLine=orddict:store(TimeSlot, Count, TimeLine),
					      [{sum, NewSum}, {tl, NewTimeLine}]
				      end,
				      [{sum, Count}, {tl, orddict:store(TimeSlot, Count, orddict:new())}], 
				      AccIn)
		  end,
		  dict:new(), Result),
    ?P(dict:to_list(ResultByGroups)),
    dict:to_list(ResultByGroups);

get_filtered_grouped_counts(StartDate, EndDate, Filters, Groups)
  when is_tuple(StartDate) and is_tuple(EndDate) ->
    get_filtered_grouped_counts({StartDate, {0,0,0}}, {EndDate, {0,0,0}}, Filters, Groups).

%% END----GROUPED-COUNTS--------------------------------------




%% START--ENTRY-COUNT-----------------------------------------

     
%% @doc gets the value and filters the records
%% this needs to return a list generated from the Value
%% each Value in the eventlog bucket is a list of #riaklogentry{} records
%% so I'm just returning the filtered Value here
mapfun_get_filtered_entry_count({error, notfound}, _KD, _Arg)  -> [];
mapfun_get_filtered_entry_count({error, not_found}, _KD, _Arg) -> [];
mapfun_get_filtered_entry_count(ValueObject, _KeyData, Filters)    -> 
    LogEntryGroup=binary_to_term(riak_object:get_value(ValueObject)),
    Entries=lists:filter(fun(Entry)->run_filters(Filters, Entry) end, LogEntryGroup),
    [length(Entries)].


-spec reducefun_get_filtered_entry_count/2 :: (Counts :: [list()], Arg :: term()) -> list().
reducefun_get_filtered_entry_count(Counts, _Arg)->
    [ lists:sum( lists:flatten(Counts) ) ].

%% @doc gets the logentries from the eventlog bucket between StartDateTime and EndDateTime and filters them according to Filters
-spec get_filtered_entry_count/3 :: (StartDateTime :: tuple(), 
				     EndDateTime :: tuple(), 
				     Filters :: [{FieldDef :: fielddef(), Regexp :: string()}])-> [#riaklogentry{}] .
get_filtered_entry_count(StartDateTime={SD,ST}, EndDateTime={ED,ET}, Filters)
  when is_tuple(SD) and is_tuple(ST) and is_tuple(ED) and is_tuple(ET) ->

    Map=fun
	    ({error, notfound}, _KD, _Arg)  -> [];
	    ({error, not_found}, _KD, _Arg) -> [];
	    (ValueObject, _KeyData, Filters)    -> 
		LogEntryGroup=binary_to_term(riak_object:get_value(ValueObject)),
		Entries=lists:filter(fun(Entry)->run_filters(Filters, Entry) end, LogEntryGroup),
		[length(Entries)]
	end,
    
    Reduce=fun(Counts, _Arg)->
		   [ lists:sum( lists:flatten(Counts) ) ]
	   end,

    {ok, RCPid}=riakc_pb_socket:start_link('127.0.0.1', 8087),

    Inputs=[ {<<"eventlog">>,K} || K <- get_key_list(StartDateTime, EndDateTime) ],

    Query=[ {map, {qfun, Map}, Filters, false},
	    {reduce, {qfun, Reduce}, none, true}],

    {ok, [{1,[Result]}]}=riakc_pb_socket:mapred(RCPid, Inputs, Query),
    riakc_pb_socket:stop(RCPid),
    Result;

get_filtered_entry_count(StartDate, EndDate, Filters)
  when is_tuple(StartDate) and is_tuple(EndDate) ->
    get_filtered_entry_count({StartDate, {0,0,0}}, {EndDate, {0,0,0}}, Filters).

%% END----ENTRY-COUNT-----------------------------------------

