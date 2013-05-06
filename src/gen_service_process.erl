-module(gen_service_process).
-behaviour(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_fsm callbacks:
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3, code_change/4]).

%% api:
-export([start_link/0]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:debug("~p: started as ~p", [?MODULE, self()]),
    {ok, stopped, #state{}}.

pid() ->
    whereis(?MODULE).

start() ->
    gen_fsm:sync_send_event(?MODULE, start).

stop() ->
    gen_fsm:sync_send_event(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stopped(start, _, State) ->
    {reply, ok, started, State}.

started(stop, _, State) ->
    {reply, ok, stopped, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(_Event, StateName, State) ->
    lager:warning("~p: unhandled event for all states: ~p", [?MODULE, _Event]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _, StateName, State) ->
    lager:warning("~p: unhandled sync event for all states: ~p", [?MODULE, _Event]),
    {next_state, StateName, State}.

handle_info({login, {From, Ref}, [UserID, ConnPid, Options]}, started, State) ->
    Result = supervisor:start_child(gen_service_conn_sup, [UserID, ConnPid, Options]),
    From ! {Ref, Result},
    {next_state, started, State};

handle_info({login, {From, Ref}, _}, stopped, State) ->
    From ! {Ref, {error, service_stopped}},
    {next_state, stopped, State}.

terminate(_Reason, _, _) ->
    lager:debug("~p: terminated with reason ~p", [?MODULE, _Reason]).

code_change(_, StateName, State, _) ->
    {ok, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
