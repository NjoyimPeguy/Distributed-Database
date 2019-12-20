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
-export([start_connection/1]).

start_connection(ExistingNode) ->
  io:format("Welcome to the distributed databases client~n"),
  ServerList = get_server_lists(ExistingNode),
  case length(ServerList) =/= 0 of
    true ->
      processing(ServerList);
    false -> ok
  end,
  io:format("Goodbye~n"),
  init:stop().


processing(ServerList) ->
  % First taking the client's query,
  % then converting the client's query to lower cases.
  CommandLine = io:get_line("Query> "),
  NextServerList = sendQuery(CommandLine, ServerList),
  case NextServerList =:= exit of
    true -> do_nothing;
    false -> processing(NextServerList)
  end.
%%--------------------------------------------------------------------------
%% get_server_lists(Node) -> [pid()]
%%         Node = atom()
%%
%% Description:
%%   1. Connects the client to server by specifying its node name.
%%   2. Returns the list of active servers in the network.
%%--------------------------------------------------------------------------
get_server_lists(ExistingNode) ->
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
sendQuery(_, []) -> exit;
sendQuery(CommandLine, [AliveServer | RemServers]) ->
  % Splitting the client's query.
  Query = re:split(string:trim(CommandLine), "\s+"),
  % Checking each argument
  case lists:nth(1, Query) of
    <<"connect">> ->
      case length(Query) == 2 of
        true ->
          ExistingNode = lists:nth(2,  Query),
          NextServerList = get_server_lists(binary_to_atom(ExistingNode, utf8)),
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
          Entry = lists:nth(2, Query),
          AliveServer ! {read, self(), Entry},
          receive
            {readGranted, ServerPid, SearchedEntry, Data} ->
              case Data =/= none of
                true ->
                  io:format("CLIENT [~p] : is reading data entry {~s, ~s} received from SERVER [~p]~n", [self(), SearchedEntry, Data, node(ServerPid)]),
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
      case length(Query) >= 2  of
        true ->
          [_ | ValueTokens] = [binary_to_list(Token) || Token <- Query],
          Value = string:join(ValueTokens, " "),
          AliveServer ! {write , self() , Value},
          receive
            {write_successful, Key} ->
              io:format("CLIENT [~p] : has just received the Key : ~s from SERVER [~p] after writting the data '~s'~n", [self(), Key, node(AliveServer), Value]),
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
      io:format("CLIENT [~p] stops requesting data to the SERVER [~p] for now.~n",  [self(), AliveServer]),
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
  ServerList = get_server_lists(node(Server)),
  case length(ServerList) =:= 0 of
    true ->
      onConnectionLost(node(Rem));
    false ->
      ServerList
  end.





