-module(db_list).
-compile(export_all).

start() ->
	Default = [{petr, moscow}, {marina, nn}, {tom, new_york}],
	Server_Pid = spawn(db_list, loop, [Default]),
	register(server, Server_Pid),
	ok.

stop() -> 
	server ! stop.

find(Location, Data, List) ->
	case lists:keyfind(Location, 2, Data) of
		{Name, Location} ->
			find(Location, lists:keydelete(Location, 2, Data), [Name | List]);
		false ->
			lists:delete([], List)
	end.

make_a_list([], List, _) ->
	lists:delete([], List),
	lists:usort(List);

make_a_list([{Name, _} | Rest], List, name) ->
	make_a_list(Rest, [Name | List], name);

make_a_list([{_, Location} | Rest], List, location) ->
	make_a_list(Rest, [Location | List], location).

loop(Data) ->
	receive
		stop ->
			server_terminated,
			exit("Finish");

		{From, sort, Chooser} ->
			List = make_a_list(Data, [], Chooser),
			From ! {ok, List},
			loop(Data);				

		{From, Location} ->
			case lists:keyfind(Location, 2, Data) of
				{_, Location} ->
					List = find(Location, Data, []),
					From ! {ok, List},
					loop(Data);
				false ->
					From ! noone,
					loop(Data)
			end;	

		{From, Name, find} ->
			case lists:keyfind(Name, 1, Data) of
				{Name, Location} ->
					From ! Location,
					loop(Data);
				false ->
					From ! {no_such_name, Name},
					loop(Data)
			end;	

		{From, Name, remove} ->
			case lists:keydelete(Name, 1, Data) of
				NewData ->
					From ! ok,
					loop(NewData)
			end;				

		{From, Name, Location} ->
			case lists:member({Name, Location}, Data) of
				true ->
					From ! no,
					loop(Data);
				false ->
					From ! yes,
					loop([{Name, Location} | Data])
			end
	end.
		 
insert(Name, Location) -> 
	server ! {self(), Name, Location},
	
	receive 
		yes ->
			ok;
		no ->
			already_inserted
	end.

where_is(Name) ->
	server ! {self(), Name, find},

	receive
		Location ->
			Location;
		{no_such_name, Name} ->
			{no_such_name, Name}
	end.

remove(Name) ->
	server ! {self(), Name, remove},

	receive 
		ok ->
			ok
	end.

located_at(Location) ->
	server ! {self(), Location}, 

	receive 
		{ok, Names} ->
			Names;
		noone ->
			noone
	end.	

all_names() ->
	server ! {self(), sort, name},
	
	receive 
		{ok, Names} ->
			Names
	end.	
	
all_locations() ->
	server ! {self(), sort, location},

	receive 
		{ok, Locations} ->
			Locations
	end.