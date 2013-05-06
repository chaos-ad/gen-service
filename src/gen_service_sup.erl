-module(gen_service_sup).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, { {rest_for_one, 5, 10}, [
        {gen_service_conn_sup, {gen_service_conn_sup, start_link, []},
            permanent, 60000, supervisor, [gen_service_conn_sup]},
        {gen_service_process, {gen_service_process, start_link, []},
            permanent, 60000, worker, [gen_service_process]},
        {gen_service_announcer, {gen_service_announcer, start_link, []},
            permanent, 60000, worker, [gen_service_announcer]}
    ]} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
