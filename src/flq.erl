%%% File    : flq.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : fixed length queue
%%% Created : 27 Nov 2010 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

%%% @doc fixed length queue

-module(flq).

-export([new/1, new/2, in/2, out/1, to_list/1]).

new(Length)->
    {Length, queue:new()}.

new(Length, InitItem)->
    {Length, queue:from_list( lists:duplicate(Length, InitItem) )}.

in(Item, {Length, Queue})->
    Q2=queue:in(Item, Queue),
    case queue:len(Q2)>Length of
	true  ->
	    {{value, _I}, Q3}=queue:out(Q2),
	    {ovrflw, {Length, Q3}};
	false  ->
	    {ok, {Length, Q2}}
    end.

out({Length, Queue})->
    case queue:out(Queue) of
	{{value, Item}, Q2} ->
	    {ok, Item, {Length, Q2}};
	{empty, Q2} ->
	    {error, empty, {Length, Q2}}
    end.

    
to_list({_Length, Queue})->
    queue:to_list(Queue).

