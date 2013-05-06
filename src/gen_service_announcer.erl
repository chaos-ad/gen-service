-module(gen_service_announcer).
-behaviour(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_fsm callbacks:
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3, code_change/4]).

%% api:
-export([start_link/0]).
-export([disconnected/2, connected/2, idle/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {conn, path}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_fsm:send_event_after(0, connect),
    {ok, disconnected, #state{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% trying to move to connected state at each timeout event
disconnected(connect, State) ->
    try
        Conn = connect(),
        gen_fsm:send_event_after(0, announce),
        {next_state, connected, #state{conn=Conn}}
    catch
        _:disconnected ->
            lager:warning("Zookeeper is unreachable"),
            gen_fsm:send_event_after(5000, connect),
            {next_state, disconnected, State#state{conn=undefined}}
    end.

connected(announce, State=#state{conn=Conn}) ->
    Name = get_name(),
    Type = get_type(),
    Data = get_data(),
    Prefix = get_prefix(),
    try
        ok   = ensure_created(Conn, Prefix),
        Path = create(Conn, filename:join(Prefix, Name), Data, Type),
        ok   = subscribe(Conn, Path),
        lager:info("Zookeeper znode '~s' was created", [Path]),
        start_service(),
        {next_state, idle, State#state{path=Path}}
    catch
        _:dir_exists ->
            lager:info("Zookeeper znode '~s' is already registered", [filename:join(Prefix, Name)]),
            gen_fsm:send_event_after(5000, announce),
            {next_state, connected, State};
        _:disconnected ->
            lager:warning("Zookeeper is unreachable"),
            gen_fsm:send_event_after(5000, connect),
            {next_state, disconnected, State#state{conn=undefined}}
    end.

idle({watchlost, _, {_, Path}}, State=#state{path=Path}) ->
    lager:warning("Zookeeper is unreachable"),
    ok = stop_service(),
    gen_fsm:send_event_after(5000, connect),
    {next_state, disconnected, State#state{conn=undefined}};

idle({znode_changed,{Path,node_deleted,_}}, State=#state{path=Path}) ->
    lager:warning("Zookeeper znode '~s' was deleted", [Path]),
    ok = stop_service(),
    gen_fsm:send_event_after(5000, announce),
    {next_state, connected, State#state{path=undefined}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(_Event, StateName, State) ->
    lager:warning("Unhandled event for all states: ~p", [_Event]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _, StateName, State) ->
    lager:warning("Unhandled sync event for all states: ~p", [_Event]),
    {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    ?MODULE:StateName(_Info, State).

terminate(_Reason, _, _) ->
    lager:debug("Announcer terminated: ~p", [_Reason]).

code_change(_, StateName, State, _) ->
    {ok, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions:

get_prefix() ->
    {ok, ServiceID} = application:get_env(service_id), 
    {ok, ZNodePrefix} = application:get_env(zookeeper_prefix), 
    filename:join([ZNodePrefix, ServiceID]).

get_name() ->
    case application:get_env(zookeeper_node) of
        {ok, Node} -> Node;
        undefined  -> atom_to_list(node())
    end.

get_data() ->
    term_to_binary([
        {node, erlang:node()},
        {cookie, erlang:get_cookie()},
        {pid, gen_service_process:pid()}
    ]).

get_type() ->
    case application:get_env(zookeeper_sequential) of
        {ok, true} -> es;
        _          -> e
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect() ->
    case application:get_env(zookeeper_servers) of
        {ok, List} -> connect(List);
        undefined  -> connect([])
    end.

connect(ZooServers) ->
    case ezk_connection_manager:start_connection(ZooServers, [self()]) of
        {ok, Conn} -> Conn;
        {error, no_server_reached} -> error(disconnected)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subscribe(Conn, Path) ->
    case ezk:exists(Conn, fix(Path), self(), znode_changed) of
        {ok, _} -> ok;
        {error, {no_zk_connection, _}} -> error(disconnected);
        {error, Error} -> error(Error)
    end.

ensure_created(Conn, Path) ->
    case ezk:ensure_path(Conn, fix(Path)) of
        {ok, _} -> ok;
        {error, {no_zk_connection, _}} -> error(disconnected);
        {error, Error} -> error(Error)
    end.

create(Conn, Path, Data, Type) ->
    case ezk:create(Conn, fix(Path), Data, Type) of
        {ok, ResultingPath} -> ResultingPath;
        {error, {no_zk_connection, _}} -> error(disconnected);
        {error, Error} -> error(Error)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fix(Path) ->
    string:strip(Path, right, $/).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_service() ->
    ok = gen_service_process:start().

stop_service() ->
    ok = gen_service_process:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
