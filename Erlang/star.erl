-module(four3).
-export([start/3, listen/0]).

start(N, M, Msg) ->
	Main = spawn(four3, listen, []),
	List = create_process([], N-1),
	action(Msg, M, Main, List).

create_process(Proc_list, 0) ->
	lists:append(Proc_list, [hd(Proc_list)]);

create_process(Proc_list, N) ->
	create_process([spawn(four3, listen, [])|Proc_list], N-1).

action(_, 0, Main, List) ->
	Main ! {stop, List};

action(Msg, M, Main, List) ->
	Main ! {Msg, M, Main, List, List}.

listen() ->
	receive
		{stop, [Next | Rest]} ->
			io:format("~p was received (~p)~n", [self(), stop]),
			Next ! {stop, Rest};
		{Msg, M, Main, [_ | []] ,List} ->
			io:format("One message was sent over!~n", []),
			action(Msg, M-1, Main, List),
			listen(); 
		{Msg, M, Temp, List, Main, delimiter} ->
			io:format("~p sends (~p) to ~p through ~p~n", [self(), Msg, hd(Temp), Main]),			
			Main ! {Msg, M, Main, Temp, List},
			listen();
		{Msg, M, Main, [Next | Rest], List} ->
			io:format("~p sends (~p) to ~p~n", [self(), Msg, Next]),
			Next ! {Msg, M, Rest, List, Main, delimiter},
			listen()
	end.
