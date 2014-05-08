-module(db_distr).
-include("db_record.hrl").
-define(SERVER_NODE, 'print_the_name_of_the_server_node@print_the_host_name').
-compile(export_all).

% SERVER'S FUNCTIONS

start_server() ->
	spawn(?MODULE, to_start_server, []).

to_start_server() ->
	Table = ets:new(table, [public, {keypos, #person.name}]),
	register(server, self()),
	loop(Table).

stop_server() -> 
	server ! stop.

loop(Table) ->
	receive		
		stop ->
			ets:delete(Table),
			exit("Finish");

		{From, Node, sort, Chooser} ->
			case Chooser of
				name ->
					ToCompare = #person{name = '$1', location = '_', company = '_'},
					List = ets:match(Table, ToCompare),
					{From, Node} ! {ok, lists:sort(List)};
				location -> 
					ToCompare = #person{name = '_', location = '$1', company = '_'},
					List = ets:match(Table, ToCompare),
					{From, Node} ! {ok, lists:usort(List)}
			end,
			loop(Table);				

		{From, Node, Location, located_at} ->
			case ets:match(Table, #person{name = '$1', location = Location, company = '_'}) of
				[] ->
					{From, Node} ! noone;
				List ->
					{From, Node} ! {ok, List}
			end, 
			loop(Table);	
		
		{From, Node, Company, working_at} ->
			case ets:match(Table, #person{name = '$1', location = '_', company = Company}) of
				[] ->
					{From, Node} ! noone;
				List ->
					{From, Node} ! {ok, List}
			end, 
			loop(Table);	

		{From, Node, Name, find} ->
			case ets:lookup(Table, Name) of
				[] ->
					{From, Node} ! {no_such_name, Name};
				[P] ->
					{From, Node} ! P#person.location
			end,
			loop(Table);

		{From, Node, Name, remove} ->
			ets:delete(Table, Name),
			{From, Node} ! ok,
			loop(Table);

		{From, Node, Name, Location, Company} ->
			case ets:member(Table, Name) of
				true ->
					{From, Node} ! no;
				false ->
					ets:insert(Table, #person{name = Name, location = Location, company = Company}),
					{From, Node} ! yes
			end,
			loop(Table)
	end.

% CLIENT'S FUNCTIONS
	 
start_client() ->
	case net_adm:ping(?SERVER_NODE) of
		pong ->
			io:format("Connection to Node is established!~n", []),
			to_start_client();
		pang ->
			io:format("The SERVER node doesn't exist!~n", [])
	end.

loop_monitor() ->
	monitor(process, {server, ?SERVER_NODE}),

	receive 
		{'DOWN', _, _, _, _} ->
			io:format("Connection to DATABASE is missed or not created!~n", []),
			stop_client()
	end.

to_start_client() ->
	
	register(client, spawn(?MODULE, loop_client, [])),
	register(monitor, spawn_link(?MODULE, loop_monitor, [])).

loop_client() ->
	receive
		stop ->
			exit("Finish");			

		yes ->
			io:format("ok~n", []),
			loop_client();
		
		no ->
			io:format("already_inserted~n", []),
			loop_client();
		
		{no_such_name, Name} ->
			io:format("{no_such_name, ~p}~n", [Name]),
			loop_client();

		ok ->
			io:format("ok~n", []),
			loop_client();

		noone ->
			io:format("noone~n", []),
			loop_client();
 
		{ok, List} ->
			io:format("~p~n", [List]),
			loop_client();

		Location ->
			io:format("~p~n", [Location]),
			loop_client()
	end. 

stop_client() ->
	client ! stop.

insert(Name, Location, Company) -> 
	{server, ?SERVER_NODE} ! {client, node(), Name, Location, Company},
	end_operation.

where_is(Name) ->
	{server, ?SERVER_NODE} ! {client, node(), Name, find},
	end_operation.

remove(Name) ->
	{server, ?SERVER_NODE} ! {client, node(), Name, remove},
	end_operation.

located_at(Location) ->
	{server, ?SERVER_NODE} ! {client, node(), Location, located_at},
	end_operation.

working_at(Company) ->
	{server, ?SERVER_NODE} ! {client, node(), Company, working_at},
	end_operation.
	
all_names() ->
	{server, ?SERVER_NODE} ! {client, node(), sort, name},
	end_operation.
	
all_locations() ->
	{server, ?SERVER_NODE} ! {client, node(), sort, location},
	end_operation.
