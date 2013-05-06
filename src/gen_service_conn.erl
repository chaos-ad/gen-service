-module(gen_service_conn).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {service_id, conn_pid, user_id, encoding, mod, mod_state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(UserID, ConnPid, Options) ->
    gen_server:start_link(?MODULE, [UserID, ConnPid, Options], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([UserID, ConnPid, Options]) ->
    erlang:monitor(process, ConnPid),
    Encoding = proplists:get_value(encoding, Options),
    Options2 = proplists:delete(encoding, Options),
    {ok, ServiceID} = application:get_env(service_id),
    {ok, Mod} = application:get_env(callback_module),
    case Mod:init(UserID, Options2) of
        {ok, ModState} ->
            {ok, #state{
                mod=Mod,
                mod_state=ModState,
                user_id=UserID,
                conn_pid=ConnPid,
                encoding=Encoding,
                service_id=list_to_binary(ServiceID)
            }};
        {error, Reason} ->
            {stop, {error, Reason}}
    end.

handle_call(_Call, _, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({request, {From, Ref}, Request}, State=#state{mod=Mod}) ->
    DecodedRequest = Mod:decode(State#state.encoding, {request, Request}),
    case Mod:handle_request(DecodedRequest, State#state.mod_state) of
        {ok, NewModState} ->
            From ! {Ref, ok},
            {noreply, State#state{mod_state=NewModState}};
        {ok, Response, NewModState} ->
            EncodedResponse = Mod:encode(State#state.encoding, {response, Response}),
            From ! {Ref, {ok, EncodedResponse}},
            {noreply, State#state{mod_state=NewModState}};
        {error, Error} ->
            From ! {Ref, {error, Error}},
            {noreply, State}
    end;

handle_info({event, Event}, State=#state{mod=Mod}) ->
    EncodedEvent = Mod:encode(State#state.encoding, {event, Event}),
    State#state.conn_pid ! {event, State#state.service_id, EncodedEvent},
    {noreply, State};

handle_info({'DOWN', _, _, _, Reason}, State) ->
    {stop, Reason, State}.

terminate(Reason, #state{mod=Mod, mod_state=ModState}) ->
    Mod:terminate(Reason, ModState).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
