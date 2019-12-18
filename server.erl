%%%-------------------------------------------------------------------
%%% @author Peguy
%%% @doc
%%%   To start a database in a new network:
%%%     server:start()
%%%
%%%   To start and add a database into an existing network:
%%%     server:join(node), where 'node' is the node name of any database in the network
%%% @end
%%% Created : 06. Dec 2019 15:18
%%%-------------------------------------------------------------------

-module(server).
-author("Peguy").
-export([init/0 , join/1]).

% Starts a new database network
init() ->
  register(main_pid , self()) ,
  io:format("Starting the first new database network at node ~p~n" , [node()]) ,
  Database = oxygen_db:new() ,
  main([] , 0 , Database).

% Joins a existing database network
join(ExistingNode) ->
  register(main_pid , self()) ,
  Pid = rpc:call(ExistingNode , erlang , whereis , [main_pid]) ,
  Pid ! {get_servers , self()} ,
  receive
    Servers ->
      io:format("A new database wants to joined the database network ~p~n" , [Servers]) ,
      join_servers(Servers) ,
      Database = oxygen_db:new() ,
      main(Servers , 0 , Database)
  end.

join_servers([]) -> done;
join_servers([Pid | Tail]) ->
  erlang:monitor(process , Pid) ,
  % Sends to each and every active database in the network
  % that the new database wants to join the network.
  Pid ! {join , self()} ,
  join_servers(Tail).

% Main server loop
main(Servers , Counter , Database) ->
  io:format("active databases: ~p~n" , [[self() | Servers]]) ,
  receive
    {get_servers , NewServerPid} ->
      io:format("Database [~p] has requested the list of the current active databases~n" , [NewServerPid]) ,
      NewServerPid ! Servers ++ [self()] ,
      main(Servers , Counter , Database);

    {join , NewServerPid} ->
      io:format("A new database (~p) has joined the network~n" , [node(NewServerPid)]) ,
      erlang:monitor(process , NewServerPid) ,
      main(Servers ++ [NewServerPid] , Counter , Database);% updating the list of current active databases.

    {'DOWN' , _ , process , NewServerPid , Reason} ->
      io:format("An active database (~p) has left the network (~p)~n" , [NewServerPid , Reason]) ,
      main(lists:delete(NewServerPid , Servers) , Counter , Database);

    {ask , ServerPidRequesting , Key} ->
      LocalData = oxygen_db:read(Key , Database) ,
      ServerPidRequesting ! {self() , LocalData} ,
      main(Servers , Counter , Database);

    {write , ClientPidRequesting , Value} ->
      io:format("SERVER [~p] : received request from client [~p] to write data :~s~n" , [self() , node(ClientPidRequesting) , Value]) ,
      Key = lists:flatten(io_lib:format("~s:~w" , [pid_to_list(ClientPidRequesting) , Counter])) ,
      NewDatabase = oxygen_db:write(Key , Value , Database) ,
      io:format("Adding in the database {Key = ~s, Value = ~s}~n" , [Key , Value]) ,
      ClientPidRequesting ! {write_successful , Key} ,
      NewCounter = Counter + 1 ,
      main(Servers , NewCounter , NewDatabase);

    {read , ClientPidRequesting , SearchedEntry} ->
      io:format("SERVER [~p] : received request from client [~p] to read data~n" , [self() , node(ClientPidRequesting)]) ,
      % This server starts searching the entry within its own database
      LocalData = oxygen_db:read(SearchedEntry , Database) ,
      case LocalData =/= none of
        true ->
          % If the entry was found, then just return to the client.
          ClientPidRequesting ! {readGranted , self() , SearchedEntry , LocalData};
        false ->
          io:format("SERVER [~p] : is aking to other databases...~n" , [self()]),
            % Otherwise ask every other client if they have the wanted entry
            {RemoteServer, RemoteData} = getRemoteData(SearchedEntry , Servers),
          case RemoteData =/= none of
            true ->
              io:format("SERVER [~p] : is updating its database...~n" , [self()]),
              NewDatabase = oxygen_db:write(SearchedEntry , RemoteData , Database),
              main(Servers, Counter, NewDatabase);
            false ->
               ClientPidRequesting ! {readGranted , RemoteServer , SearchedEntry , none}
          end
      end ,
      main(Servers , Counter , Database)
  end.

getRemoteData(_ , []) -> none;
getRemoteData(SearchedEntry , [OtherServer | Tail]) ->
  % Upon receiving a request from a given client,
  % This server/node has to send a message to every other node,
  % to get the searched data.
  OtherServer ! {ask , self() , SearchedEntry} ,
  receive
    {RemoteServer , ReturnedEntry} ->
      case undefined =:= ReturnedEntry of
        true ->
          getRemoteData(SearchedEntry , Tail);
        false ->
          {RemoteServer , ReturnedEntry}
      end
  end.