%%% File    : restricted_shell_callback.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : callback for restricted shell used by the clients
%%% Created : 12 Feb 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(restricted_shell_callback).


-export([local_allowed/3, non_local_allowed/3]).

local_allowed(Func, ArgList, State) -> {false,State}.

non_local_allowed(FuncSpec, ArgList, State) -> {false,State}.
