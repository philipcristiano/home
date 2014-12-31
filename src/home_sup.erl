-module(home_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{home_tsdb,
              {home_tsdb_server, start_link, []},
               permanent, 5000, worker, [home_tsdb]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
