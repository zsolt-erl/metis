-module (setuid).
-author ('sergey.miryanov@gmail.com').

-behaviour (gen_server).

%% API
-export ([setuid/1, setgid/1, seteuid/1, setegid/1]).
-export ([getuid/0, getgid/0, geteuid/0, getegid/0]).

%% gen_server callbacks
-export ([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% Internal
-export ([start_link/0]).

-define ('CMD_SET_UID',       1).
-define ('CMD_SET_GID',       2).
-define ('CMD_SET_EUID',      3).
-define ('CMD_SET_EGID',      4).
-define ('CMD_GET_UID',       51).
-define ('CMD_GET_GID',       52).
-define ('CMD_GET_EUID',      53).
-define ('CMD_GET_EGID',      54).
-define ('CMD_FORMAT_ERRNO',  100).

%% API
setuid (UID) when is_integer (UID) ->     gen_server:call (setuid, {set, ?CMD_SET_UID, UID}).
setgid (GID) when is_integer (GID) ->     gen_server:call (setuid, {set, ?CMD_SET_GID, GID}).
seteuid (EUID) when is_integer (EUID) ->  gen_server:call (setuid, {set, ?CMD_SET_EUID, EUID}).
setegid (EGID) when is_integer (EGID) ->  gen_server:call (setuid, {set, ?CMD_SET_EGID, EGID}).

getuid () ->  gen_server:call (setuid, {get, ?CMD_GET_UID}).
getgid () ->  gen_server:call (setuid, {get, ?CMD_GET_GID}).
geteuid () -> gen_server:call (setuid, {get, ?CMD_GET_EUID}).
getegid () -> gen_server:call (setuid, {get, ?CMD_GET_EGID}).

%% --------------------------------------------------------------------
%% @spec start_link () -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts driver
%% @end
%% --------------------------------------------------------------------
-type (result () :: {'ok', pid ()} | 'ignore' | {'error', any ()}).
-spec (start_link/0::() -> result ()).
start_link () ->
  gen_server:start_link ({local, setuid}, ?MODULE, [], []).

-type(init_return() :: {'ok', tuple()} | {'ok', tuple(), integer()} | 'ignore' | {'stop', any()}).
-spec(init/1::([]) -> init_return()).
init ([]) ->
  process_flag (trap_exit, true),
  SearchDir = filename:join ([filename:dirname (code:which (?MODULE)), "..", "ebin"]),
  case erl_ddll:load (SearchDir, "setuid_drv")
  of
    ok -> 
      Port = open_port ({spawn, "setuid_drv"}, [binary]),
      {ok, Port};
    {error, Error} ->
      {stop, string:join (["Error loading setuid driver: ", erl_ddll:format_error (Error)], "")}
  end.

%% --------------------------------------------------------------------
%% @spec code_change (OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%% @hidden
%% --------------------------------------------------------------------
code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages.
%% @end
%% @hidden
%% --------------------------------------------------------------------
handle_cast (_Msg, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages.
%% @end
%% @hidden
%% --------------------------------------------------------------------
handle_info (_Info, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any 
%% necessary cleaning up. When it returns, the gen_server terminates 
%% with Reason.
%%
%% The return value is ignored.
%% @end
%% @hidden
%% --------------------------------------------------------------------
terminate (normal, Port) ->
  port_command (Port, term_to_binary ({close, nop})),
  port_close (Port),
  ok;
terminate (_Reason, _State) ->
  ok.

%% --------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages.
%% @end
%% @hidden
%% --------------------------------------------------------------------
handle_call ({set, Command, ID}, _From, Port) ->
  Reply = format_result (Port, control_drv (Port, Command, int_to_binary (ID))),
  {reply, Reply, Port};
handle_call ({get, Command}, _From, Port) ->
  Reply = control_drv (Port, Command),
  {reply, Reply, Port};
handle_call (Request, _From, Port) ->
  {reply, {unknown, Request}, Port}.

control_drv (Port, Command) when is_port (Port) and is_integer (Command) ->
  port_control (Port, Command, <<>>),
  wait_result (Port).

control_drv (Port, Command, Data) 
  when is_port (Port) and is_integer (Command) and is_binary (Data) ->
    port_control (Port, Command, Data),
    wait_result (Port).

wait_result (_Port) ->
  receive
	  Smth -> Smth
  end.

format_result (_Port, {ok}) ->
  {ok};
format_result (Port, {error, Errno}) ->
  {error, control_drv (Port, ?CMD_FORMAT_ERRNO, int_to_binary (Errno))}.


int_to_binary (Int) ->
  erlang:list_to_binary (erlang:integer_to_list (Int)).

