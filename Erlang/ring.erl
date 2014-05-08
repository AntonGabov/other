-module(test).
-export([start/2, listen/0, send/3]).

start(N,M) ->
	List = [{X, spawn(?MODULE, listen, [])} || X <- lists:seq(1,N)],
	send(M, N, List).

send(1, 0, _List) ->
	ok;

send(M, 0, List) ->
	{Pos, _Pid} = lists:last(List),
	send(M-1, Pos, List);

send(M, N, List) ->
	{Pos, Pid} = lists:keyfind(N, 1, List),
	Pid ! {Pid, Pos, M},
	send(M, N-1, List).

listen() ->
	receive
		{Pid, Pos, M} ->
			io:format("Hello, I am (~p), my position: ~p. It is only ~p iteration~n", [Pid, Pos, M]),
			case M of
				1 -> terminate;
				_Any -> listen()
			end
	end.
