%%% File    : misc.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : misc. helper functions
%%% Created : 17 Dec 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(misc).
-compile(export_all).

datetime_to_string({{Year,Month,Day},{Hour,Min,Sec}})->
    YearStr=   string:right( integer_to_list(Year), 4, $0 ),
    MonthStr=  httpd_util:month(Month),
    DayStr=    string:right( integer_to_list(Day), 2, $0 ),
    WeekDayStr=httpd_util:day( calendar:day_of_the_week(Year,Month,Day) ),
    HourStr=   string:right( integer_to_list(Hour), 2, $0 ),
    MinStr=    string:right( integer_to_list(Min), 2, $0 ),
    SecStr=    string:right( integer_to_list(Sec), 2, $0 ),
    DayStr++" "++MonthStr++" "++YearStr++" "++HourStr++":"++MinStr++":"++SecStr.


%% helper function to count distibution of data in a set
%% eg. set is [a,b,c,a,d] the distibution is [{a,2},{b,1},{c,1},{d,1}]
%% this would be called for each datum
cnt_distrib(Data, Distrib)->
    NewDistrib=case lists:keyfind(Data, 1, Distrib) of
		false->[{Data,1}|Distrib];
		{Data, Amount}-> lists:keyreplace(Data, 1, Distrib, {Data, Amount+1})
	    end.

%% same as above using an ETS table
cnt_distrib_ets(Data, TableId)->
    case ets:member(TableId, Data) of
	false -> ets:insert(TableId, {Data, 1});
	true  -> ets:update_counter(TableId, Data, 1)
    end,
    TableId.


multistrip(Str, Chars)->
    Pred=fun(Elem)->lists:member(Elem,Chars) end,
    Pass1=lists:dropwhile(Pred, Str),
    Pass2=lists:dropwhile(Pred, lists:reverse(Pass1)),
    lists:reverse(Pass2).
			      
		 
list_to_term(List)->
    {ok, Tokens, EndLoc}=erl_scan:string(List++"."),
    {ok, Term}=erl_parse:parse_term(Tokens),
    Term.


%% @doc opens a port to the command and sends Data to port (as if it was coming on STDIN)
%% can't just send data to post because there's no way to close STDIN from erlang
%% Cmd needs to have absolute path, it does not search PATH env var
-spec exec/2 :: (Cmd :: string(), Data :: string()) -> [Response :: binary()].
exec(Cmd, Data)->
    Port=setup(Cmd),
    Length=length(Data),
    port_command(Port, <<Length:32/native>>),
    port_command(Port, Data),

    Response=gather_response(Port),
    port_close(Port),
    Response.

setup(Cmd)->
    Forcer = get_base_dir(?MODULE) ++ "/priv/stdin_forcer",
    open_port({spawn_executable, Forcer},
	      [stream, use_stdio, stderr_to_stdout, binary, eof,
	       {args, string:tokens(Cmd, " ")}]).

get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).

gather_response(Port) ->
  gather_response(Port, []).
gather_response(Port, Accum) ->
  receive
    {Port, {data, Bin}} -> gather_response(Port, [Bin | Accum]);
    {Port, eof} -> lists:reverse(Accum)
  after % max 30 seconds of time for the process to send EOF (close stdout)
    30000 -> {died, lists:reverse(Accum)}
  end.

join(Things, With)->
    T=fun
	  (Thing) when is_atom(Thing)  -> atom_to_list(Thing);
	  (Thing) when is_tuple(Thing) -> lists:flatten( io_lib:format("~p", [Thing]));
	  (Thing) when is_list(Thing)  -> Thing
      end,
    string:join( [T(Thing) || Thing <- Things], With ).
	      
