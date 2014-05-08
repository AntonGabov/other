-module(four1).
-export([start/1, pong/0, ping/2]).

start(X) ->
	Pong = spawn(four1, pong, []),
	spawn(four1, ping, [X, Pong]).

pong() ->
	receive 
		{PingId, ping} ->
			io:format("Pong~n", []),
			PingId ! pong,
			pong();

		exit ->
			io:format("Das ist Ende!~n", [])

	end.	

ping(0, Pong) ->
	Pong ! exit;
	
ping(N, Pong) ->
	Pong ! {self(), ping},
	
	receive
		pong ->
			io:format("Ping~n", [])
	end,
	
	ping(N - 1, Pong).