-module(db_record).
-include("db_record.hrl").
-compile(export_all).

start() ->
	Server_Pid = spawn(?MODULE, loop, [[]]),
	register(server, Server_Pid),
	ok.

stop() -> 
	server ! stop.

find(Location, Data, List, located) ->
	case lists:keyfind(Location, #person.location, Data) of
		false ->
			lists:delete([], List);
		P ->
			find(Location, lists:keydelete(Location, #person.location, Data), [P#person.name | List], located)
	end;

find(Company, Data, List, working) ->
	case lists:keyfind(Company, #person.company, Data) of
		false ->
			lists:delete([], List);
		P ->
			find(Company, lists:keydelete(Company, #person.company, Data), [P#person.name | List], working)
	end.

make_a_list([], List, _) ->
	lists:delete([], List),
	lists:usort(List);

make_a_list([Head | Rest], List, name) ->
	make_a_list(Rest, [Head#person.name | List], name);

make_a_list([Head | Rest], List, location) ->
	make_a_list(Rest, [Head#person.location | List], location).

loop(Data) ->
	receive
		stop ->
			server_terminated,
			exit("Finish");

		{From, sort, Chooser} ->
			List = make_a_list(Data, [], Chooser),
			From ! {ok, List},
			loop(Data);				

		{From, Location, located_at} ->
			case lists:keyfind(Location, #person.location, Data) of
				false ->
					From ! noone;
				_ ->
					List = find(Location, Data, [], located),
					From ! {ok, List}
			end, 
			loop(Data);	
		
		{From, Company, working_at} ->
			case lists:keyfind(Company, #person.company, Data) of
				false ->
					From ! noone;
				_ ->
					List = find(Company, Data, [], working),
					From ! {ok, List}
			end, 
			loop(Data);	

		{From, Name, find} ->
			case lists:keyfind(Name, #person.name, Data) of
				false ->
					From ! {no_such_name, Name};
				P ->
					From ! P#person.location
			end,
			loop(Data);

		{From, Name, remove} ->
			case lists:keydelete(Name, #person.name, Data) of
				NewData ->
					From ! ok,
					loop(NewData)
			end;				

		{From, Name, Location, Company} ->
			Temp = #person{name = Name, location = Location, company = Company},
			case lists:member(Temp , Data) of
				true ->
					From ! no,
					loop(Data);
				false ->
					From ! yes,
					loop([Temp | Data])
			end
	end.
	 
insert(Name, Location, Company) -> 
	server ! {self(), Name, Location, Company},
	
	receive 
		yes ->
			ok;
		no ->
			already_inserted
	end.

where_is(Name) ->
	server ! {self(), Name, find},

	receive
		{no_such_name, Name} ->
			{no_such_name, Name};
			
		Location ->
			Location
	end.

remove(Name) ->
	server ! {self(), Name, remove},

	receive 
		ok ->
			ok
	end.

located_at(Location) ->
	server ! {self(), Location, located_at}, 

	receive 
		{ok, Names} ->
			Names;
		noone ->
			noone
	end.	

working_at(Company) ->
	server ! {self(), Company, working_at}, 

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
