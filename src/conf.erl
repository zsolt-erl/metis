%%%-------------------------------------------------------------------
%%% File    : conf.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%
%%% Created : 31 Dec 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

%% @doc  Configuration server
%%
%% <pre>
%% config file format:
%%    [section]
%%      key:  value
%%      INCLUDE(FileName)
%%      key2: CONSULT(FileName2)
%%  
%%  [section]     is section heading, needs to be an atom
%%  key:          key in the section, needs to be an atom
%%  INCLUDE(FileName)    include an other file of the same format (if 
%%                       there is no section heading on the top of the
%%                       include file then it continues with the same 
%%                       section)
%%                       FileName is atom or string
%%  CONSULT(FileName)    call file:consult(FileName) and use result
%%                       as Value
%%
%% NOTES: 
%% key: value pair needs to be on one line, if value is longer then
%% put it in a separate file and use CONSULT
%%
%% calls transform/0 after processing the file, this can implement any 
%% transformation that needs to be done on the ETS table
%% </pre>
%%%-------------------------------------------------------------------
-module(conf).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/1, get/2, getSection/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("metis.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [FileName], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([FileName]) ->
    ets:new(conf, [named_table]),
    process_file(FileName),
    ?info("processed file",[]),
    transform(),
    ?info("Init done",[]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, Section, Key}, _From, State)->
    Reply=
	case ets:lookup(conf, {Section, Key}) of
	    []       -> nonexkey;
	    [{_K, V}] -> V
	end,
    {reply, Reply, State};

handle_call({get_section, Section}, _From, State)->
    Reply=
	ets:foldl(fun({{S, Key}, Value}, Acc)-> 
			  case S of
			      Section -> [{Key, Value}|Acc];
			      _       -> Acc
			  end
		  end,
		  [], conf),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("CONF TERMINATING~n"),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


process_file(FileName)->
    {ok, File}=file:open(FileName, read),
    process_file(File, []),
    file:close(File).



process_file(File, Section)->
    case file:read_line(File) of
	eof ->
	    ok;
	{ok, Line}->
	    Line2=misc:multistrip(Line, " \n\r\t"),
	    NewSection=process_line(Line2, Section),
	    process_file(File, NewSection)
    end.

    
process_line([$#|_], Section)->
    Section;
process_line([], Section)->
    Section;
process_line([$[|Rest], _Section)->
    list_to_atom(string:strip(Rest, right, $]));
process_line(Line, Section) ->
    case re:run(Line, "INCLUDE\\((.*)\\)", [{capture, all_but_first, list}]) of
	{match, [IncludeFileName]} ->
	    %% need to include an other file
	    case file:open(IncludeFileName, read) of
		{ok, IncludeFd}->
		    ?debug("Including file ~p", [IncludeFileName]),
		    process_file(IncludeFd, Section),
		    file:close(IncludeFd);
		_Error ->
		    ?debug("Couldn't open ~p, file not included.", [IncludeFileName])
	    end;
	_Other ->
	    %% need to parse the line and get the key and the value
	    [KeyPart, ValuePart]=string:tokens(Line, ":"),
	    Key=list_to_atom(misc:multistrip(KeyPart, " \t")),
	    Value =
		case re:run(ValuePart, "CONSULT\\((.*)\\)", [{capture, all_but_first, list}]) of
		    {match, [ConsultFileName]} ->
			%% consult file to get the value
			case file:consult(ConsultFileName) of
			    {ok, Terms} ->
				?debug("Consulted file ~p", [ConsultFileName]),
				Terms;
			    _Error ->
				?debug("Couldn't consult ~p, value is set to {consult_error}.", [ConsultFileName]),
				{consult_error}
			end;
		    _Other->
			%% simply eval the ValuePart
			ValueTerm1=misc:list_to_term(ValuePart),
			case is_list(ValueTerm1) of
			    true ->
				%% if it looks like an ipv4 address convert it to tuple
				%% need to check correct format because inet_parse:address accepts incomplete addr  (eg. "127.1"}
				case catch re:run(ValueTerm1, "[012]{0,1}\\d{0,2}\\.[012]{0,1}\\d{0,2}\\.[012]{0,1}\\d{0,2}\\.[012]{0,1}\\d{0,2}", []) of
				    {match, _} -> 
					{ok, Parsed}=inet_parse:address(ValueTerm1),
					Parsed;
				    _NoMatchOrError -> 
					ValueTerm1
				end;
			    false ->
				ValueTerm1
			end
		end,
	    ets:insert(conf, {{Section, Key}, Value})
    end,
    Section.


transform()->
    %% create local_fqdn parameters for all nodes
    [{_, Server}]=ets:lookup(conf, {main, server}),
    [{_, Clients}]=ets:lookup(conf, {main, clients}),
    Nodes=[Server|Clients],
    lists:foreach(fun(Node)->
			  [_, FQDN]=string:tokens(atom_to_list(Node), "@"),
			  ets:insert(conf, {{Node, local_fqdn}, FQDN})
		  end, Nodes),
    
    %% convert reroute table to binary
    case ets:lookup(conf, {Server, reroute}) of
	[]       -> nonexkey;
	[{_K, V}] -> 
	    ConvertToBinary= 
		fun(Addr) when is_atom(Addr)   -> list_to_binary( atom_to_list(Addr) );
		   (Addr) when is_list(Addr)   -> list_to_binary(Addr);
		   (Addr) when is_binary(Addr) -> Addr;
		   (Addr) -> 
			?debug("Invalid value in reroute map: ~p", [Addr]),
			invalid
		end,
	    NewValue=lists:map(fun({Addr1,Addr2})-> {ConvertToBinary(Addr1), ConvertToBinary(Addr2)} end, V),
	    ets:insert(conf, {{Server, reroute}, NewValue})
    end,


    ok.


%%% -------------- API --------------

get(Section, Key)->
    gen_server:call(conf, {get, Section, Key}).

getSection(Section)->
    gen_server:call(conf, {get_section, Section}).
		      
