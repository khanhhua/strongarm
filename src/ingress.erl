-module(ingress).
-behavior(gen_server).
-export([start/2]).
-export([init/1, terminate/2, code_change/3, format_status/1, handle_call/3, handle_cast/2, handle_continue/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}]).

%% Public interface
%% TODO Normal argument layout
start(Port, HttpTimeout) ->
    info("Starting server at port: ~p", [Port]),
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, HttpTimeout], []) of
        {ok, Pid} ->
            accept(Pid),
            {ok, Pid};
        Error -> Error
    end.

%% Private functions
info(Format, Args) ->
    io:format("[INFO] " ++ Format ++ "~n", Args).

warn(Format, Args) ->
    io:format("[WARN] " ++ Format ++ "~n", Args).

accept(Pid) ->
    gen_server:cast(Pid, accept).

%% Callbacks
% Initialize the server.
% callback gen_server:init/1 requires arity of 1, therefore gotta cook all params in
init([Port, HttpTimeout]) ->
    info("ingress self: ~p", [self()]),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSock} ->
            NewState = #{ lsocket => LSock,
                          http_timeout => HttpTimeout },
            { ok, NewState };
        {error, Reason} ->
            {stop, Reason}
    end.

% Handle server termination.
terminate(Reason, State) -> ok.

% Update the server state after code change.
code_change(OldVsn, State, Extra) -> {ok, State}.

% Format/limit the status value.
format_status(State) ->
    #{state => State,
      message => "Status formatted",
      reason => "Reason",
      log => "Log"}.

% Handle a call.
handle_call(Request, From, State) -> {noreply, State}.

% Handle a cast message.
handle_cast(accept, State = #{}) ->
    #{lsocket := LSock} = State,
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            info("Client acccepted", []),
            gen_tcp:controlling_process(Sock, self());
        Other -> warn("I don't know you. ~p", [Other])
    end,
    accept(self()),
    {noreply, State};
handle_cast(Request, State) ->
    {noreply, State}.

% Handle a callback continuation.
handle_continue(Info, State) -> ok.

% Handle an info message (regular process message).
handle_info({tcp, Port, _Data}, State = #{}) ->
    info("Processing client...", []),
    gen_tcp:send(Port, <<"HTTP/1.1 200 OK\r\n">>),
    gen_tcp:close(Port),
    {noreply, State};
handle_info(Info, State) ->
    info("Inbound message at ~p: ~p", [self(), Info]),
    {noreply, State}.
