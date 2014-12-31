-module(home_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Pid} = home_sup:start_link(),
    GPPORT = application:get_env(etsdb, graphite_port, 2003),
    {ok, _RanchPid} = ranch:start_listener(graphite_input, 1,
        ranch_tcp, [{port, GPPORT}], home_graphite_protocol, []),
    {ok, Pid}.

stop(_State) ->
	ok.
