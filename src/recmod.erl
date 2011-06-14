%% @doc helper module to deal with records
%% 
%% usage:
%% <pre>
%% -record(alfa, {one, two, three}).
%%
%% R1=#alfa{one=hello, two="wire"},
%% Alfa=recmod:new(alfa, record_info(fields, alfa)),
%% io:format("one:~p~n", [Alfa:get(one, R1)]),
%% io:format("two:~p~n", [Alfa:get(two, R1)]),    
%% io:format("three:~p~n", [Alfa:get(three, R1)]),
%% R2=Alfa:set(three, R1, good),
%% io:format("R2:~p~n",[R2]),
%% io:format("Prop List:~p~n", [Alfa:toPropList(R2)]),
%% </pre>


-module(recmod, [RecName, FieldNamePos]).

%%-export([get/2, set/3, toPropList/1]).

-compile(export_all).

new(Arg1, Arg2)->
    {recmod, Arg1, [{lists:nth(I, Arg2), I} || I <- lists:seq(1, length(Arg2))]}.

toPropList(Rec)
  when element(1, Rec)==RecName ->
    [RecName|RecFields]=tuple_to_list(Rec),
    FieldNames=[FieldName || {FieldName, _Pos} <- FieldNamePos],
    lists:zip(FieldNames, RecFields).

set(FieldName, Rec, Value) 
  when element(1, Rec)==RecName ->
    {FieldName, Index}=lists:keyfind(FieldName, 1, FieldNamePos),
    setelement(Index+1, Rec, Value).


get(FieldName, Rec) 
  when element(1, Rec)==RecName ->
    {FieldName, Index}=lists:keyfind(FieldName, 1, FieldNamePos),
    element(Index+1, Rec).
