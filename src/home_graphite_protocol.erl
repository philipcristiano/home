-module(home_graphite_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport, <<"">>).

loop(Socket, Transport, Buffer) ->
	case Transport:recv(Socket, 0, 3600000) of
		{ok, Data} ->
            {ok, Unprocessed} = handle_data(Buffer, Data),
			loop(Socket, Transport, Unprocessed);
		A ->
            io:format("Graphite huh? ~p~n", [A]),
			ok = Transport:close(Socket)
	end.

handle_data(Buffer, NewData) ->
    Data = <<Buffer/binary, NewData/binary>>,
    Unprocessed = parse_data(Data),
    {ok, Unprocessed}.

parse_data(Data) ->
    case binary:split(Data, <<"\n">>) of
    [_] ->
        Data;
    [Line, Rest] ->
        write_message(Line),
        parse_data(Rest)
    end.

write_message(Messages) ->
    [Metric, Value, TS] = binary:split(Messages, [<<" ">>], [global]),
    NumTS = case erlang:binary_to_integer(TS) of
                0 -> get_timestamp();
                A -> A
    end,
    home_tsdb_server:write(Metric, NumTS, etsdb_numbers:to_float(Value)),
    ok.

get_timestamp() ->
  {Mega, Sec, _Micro} = os:timestamp(),
  (Mega*1000000 + Sec).
