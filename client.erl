%%%-------------------------------------------------------------------
%%% @author Peguy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Dec 2019 11:51
%%%-------------------------------------------------------------------
-module(client).
-author("Peguy").

%% API
-export([start/1]).

start(ExistingNode) ->
  io:format("Welcome to the distributed databases client~n"),
  ServerList = connect(ExistingNode),
  case length(ServerList) == 0 of
    true ->
      processing(ServerList);
    false -> ok
  end,
  io:format("goodbye~n"),
  init:stop().


processing(ServerList) ->
  % First taking the client's query,
  % then converting the client's query to lower cases.
  CommandLine = string:to_lower(io:get_line("Query> ")),
  NextServerList = sendQuery(CommandLine, ServerList),
  case NextServerList =:= exit of
    true -> do_nothing;
    false -> processing(NextServerList)
  end.
%%--------------------------------------------------------------------------
%% connect(Node) -> [pid()]
%%         Node = atom()
%%
%% Description:
%%   1. Connects the client to server by specifying its node name.
%%   2. Returns the list of active servers in the network.
%%--------------------------------------------------------------------------
connect(ExistingNode) ->
  ServerPid = rpc:call(ExistingNode, erlang, whereis, [main_pid]),
  case is_pid(ServerPid) of
    true ->
      ServerPid ! {get_servers, self()},
      receive
        ServerList ->
          io:format("connected to server ~s~n", [ExistingNode]),
          monitor(process, ServerPid),
          ServerList
      end;
    false ->
      io:format("cannot connect to server ~s~n", [ExistingNode]),
      []
  end.

%%--------------------------------------------------------------------------
%% sendQuery(CommandLine, Servers) -> NextServers | exit
%%             Line = string()
%%             Servers = [pid()]
%%             NextServers = [pid()]
%%
%% Description:
%%   Executes a command line with a specific database.
%%   Returns the next database to use, or 'exit'
%%   if the user has requested the program to close.
%%--------------------------------------------------------------------------
sendQuery(CommandLine, [AliveServer | RemServers]) ->
  % Splitting the client's query.
  Query = re:split(string:trim(CommandLine), "\s+"),
  % Checking each argument
  case lists:nth(1, Query) of
    <<"connect">> ->
      case length(Query) == 2 of
        true ->
          ExistingNode = lists:nth(2,  Query),
          NextServerList = connect(binary_to_atom(ExistingNode, utf8)),
          if
            length(NextServerList) =:= 0 ->
              [AliveServer | RemServers];
            true ->
              NextServerList
          end;
        false ->
          io:format("usage: connect <node>~n"),
          [AliveServer | RemServers]
      end;
    <<"read">> ->
      case length(Query) == 2  of
        true ->
          Entry = binary_to_list(lists:nth(2, Query)),
          AliveServer ! {read, self(), Entry},
          receive
            {readGranted, ServerPid, SearchedEntry, Data} ->
              case Data =/= none of
                true ->
                  io:format("CLIENT [~p] : is reading data entry {~s, ~s} received from SERVER [~p]~n", [self(), SearchedEntry, Data, ServerPid]),
                  [AliveServer | RemServers];
                false ->
                  io:format("SERVER [~p] : sorry, I could not find your data!~n", [AliveServer]),
                  [AliveServer | RemServers]
              end;
            {'DOWN', _, process, _, _} ->
              NextServerList = onConnectionLost(RemServers),
              sendQuery("read " ++ Entry, NextServerList)
          end;
        false ->
          io:format("Query usage: read 'key'~n")
      end;

    <<"write">> ->
      case length(Query) == 2  of
        true ->
          Value = binary_to_list(lists:nth(2, Query)),
          AliveServer ! {write , self() , Value},
          receive
            {write_successful, Key} ->
              io:format("CLIENT [~p] : has just received the Key '~s' from SERVER [~p] after writting the data '~s'~n", [self(), Key, AliveServer, Value]),
              [AliveServer | RemServers];
            {'DOWN', _, process, _, _} ->
              NextServerList = onConnectionLost(RemServers),
              sendQuery("read " ++ Value, NextServerList)
          end;
        false ->
          io:format("Query usage: write 'value'~n"),
          [AliveServer | RemServers]
      end;

    <<"quit">> ->
      io:format("CLIENT [~p] stops requesting data to the SERVER [~p] for now",  [self(), AliveServer]),
      exit;

    <<"">> ->
      io:format("Query usage: write 'value'~n"),
      io:format("Query usage: read 'key'~n"),
      [AliveServer | RemServers];
    _ ->
      io:format("unkown command: ~s~n", [lists:nth(1, Query)]),
      [AliveServer | RemServers]
  end.

%%--------------------------------------------------------------------------
%% onConnectionLost(Servers) -> [pid()]
%%                    Servers = [pid()]
%%
%% Description:
%%   Tries to connect to another server.
%%   Returns an empty list if no more servers are available.
%%--------------------------------------------------------------------------
onConnectionLost([]) ->
  io:format("connection to database lost~n"),
  [];

onConnectionLost([Server | Rem]) ->
  io:format("connecting to antoher server~n"),
  ServerList = connect(node(Server)),
  case length(ServerList) =:= 0 of
    true ->
      onConnectionLost(node(Rem));
    false ->
      ServerList
  end.





