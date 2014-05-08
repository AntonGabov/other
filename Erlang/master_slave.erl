-module(master_slave).
-export([start/1, to_slave/2, stop/0, create_slaves/2, listen_slaves/0]).

start(N) ->
	Master_pid = spawn(master_slave, create_slaves, [N, []]),
	register(master, Master_pid).

create_slaves(0, Slaves) ->
	process_flag(trap_exit, true),
	listen_master(Slaves);

create_slaves(N, Slaves) ->
	Pid = spawn_link(master_slave, listen_slaves, []),
	create_slaves(N-1, [{N, Pid} | Slaves]).

to_slave(Msg, Num) ->
	master ! {Msg, Num}.
	
stop() ->
	master ! stop.

find(Slaves, Num, Msg) ->
	case lists:keyfind(Num, 1, Slaves) of
            false -> 
			io:format("Slave (~p) doesn't exsist~n", [Num]);
            {Num, Pid} ->  
			Pid ! {Msg, Num}
	end.

listen_slaves() ->
	receive
		{die, _} ->
			exit("Slave is dead");		
		{Msg, Num} -> 
			io:format("Slave (~p) got message (~p)~n", [Num, Msg]),
			listen_slaves();
		{'EXIT', master, Reason} -> 
			io:format("Slave exit normal"),
            	exit(normal)
	end.

listen_master(Slaves) ->
	receive
		{Msg, Num} ->
			find(Slaves, Num, Msg),
			listen_master(Slaves);
		stop -> 
			io:format("You have killed MASTER!~n", []),
			exit("Master is dead");
		{'EXIT', Slave_pid, Reason} -> 
			{N, Old_slave} = lists:keyfind(Slave_pid, 2, Slaves),
			New_slave = spawn_link(master_slave, listen_slaves, []),
			io:format("Slave (~p) with ~p restarted with new ~p~n",[N, Old_slave, New_slave]),
			listen_master(lists:keyreplace(Slave_pid, 2, Slaves, {N, New_slave}))
	end.
	