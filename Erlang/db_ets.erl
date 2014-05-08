-module(db_ets).
-include("db_record.hrl").
-compile(export_all).

start() ->
	spawn(?MODULE, to_start_loop, []).

to_start_loop() ->
	Table = ets:new(table, [public, {keypos, #person.name}]),
	register(server, self()),
	loop(Table).

stop() -> 
	server ! stop.

loop(Table) ->
	receive
		stop ->
			ets:delete(Table),
			exit("Finish");

		{From, sort, Chooser} ->
			case Chooser of
				name ->
					ToCompare = #person{name = '$1', location = '_', company = '_'},
					List = ets:match(Table, ToCompare),
					From ! {ok, lists:sort(List)};
				location -> 
					ToCompare = #person{name = '_', location = '$1', company = '_'},
					List = ets:match(Table, ToCompare),
					From ! {ok, lists:usort(List)}
			end,
			loop(Table);				

		{From, Location, located_at} ->
			case ets:match(Table, #person{name = '$1', location = Location, company = '_'}) of
				[] ->
					From ! noone;
				List ->
					From ! {ok, List}
			end, 
			loop(Table);	
		
		{From, Company, working_at} ->
			case ets:match(Table, #person{name = '$1', location = '_', company = Company}) of
				[] ->
					From ! noone;
				List ->
					From ! {ok, List}
			end, 
			loop(Table);	

		{From, Name, find} ->
			case ets:lookup(Table, Name) of
				[] ->
					From ! {no_such_name, Name};
				[P] ->
					From ! P#person.location
			end,
			loop(Table);

		{From, Name, remove} ->
			ets:delete(Table, Name),
			From ! ok,
			loop(Table);

		{From, Name, Location, Company} ->
			%io:format("~p~n", [ets:member(Table, Name)]),
			case ets:member(Table, Name) of
				true ->
					From ! no;
				false ->
					ets:insert(Table, #person{name = Name, location = Location, company = Company}),
					From ! yes
			end,
			loop(Table)
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