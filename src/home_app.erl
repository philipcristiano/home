-module(home_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, Pid} = home_sup:start_link(),
    GPPORT = application:get_env(home, graphite_port, 2003),
    {ok, _RanchPid} = ranch:start_listener(graphite_input, 1,
        ranch_tcp, [{port, GPPORT}], home_graphite_protocol, []),
    start_cowboy(),
    {ok, Pid}.

stop(_State) ->
	ok.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", home_handler_metrics, []},
               {"/static/[...]", cowboy_static, {dir, "priv/static/"}},
               {"/metrics", home_handler_metrics, []},
               {"/metrics/:metric/", home_handler_metric_data, []}
        ]}
    ]),
    CBHTTP = application:get_env(home, http_port, 8080),
    cowboy:start_http(home_http_listener, 100, [{port, CBHTTP}],
        [{env, [{dispatch, Dispatch}]}]
    ).
