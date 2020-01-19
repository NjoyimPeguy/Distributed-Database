%%%-------------------------------------------------------------------
%%% @author Black Star
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2019 16:41
%%%-------------------------------------------------------------------
-module(monitor).
-author("Peguy").

%% API
-export([start/0]).

start() ->
  io:format("Welcome to the distributed databases monitor~n"),
  processing(null),
  io:format("goodbye~n"),
  init:stop().

processing(FromExistingNode) ->
  CommandLine = io:get_line("Command> "),
  NextExistingNode = commands(CommandLine, FromExistingNode),
  case NextExistingNode =:= null of
    true ->
      processing(null);
    false ->
      processing(NextExistingNode)
  end.

commands(CommandLine, FromExistingNode) ->
  Tokens = re:split(string:trim(CommandLine), "\s+"),
  case lists:nth(1, Tokens) of
    <<"status">> ->
      if
        FromExistingNode =:= null->
          io:format("There are no servers to print!~n"),
          FromExistingNode;
        true ->
          print_status(FromExistingNode),
          FromExistingNode
        end;

    % Create a new database network
    <<"create">> ->
      case length(Tokens) =:= 2 of
        true ->
          NodeName = lists:nth(2, Tokens),
          deploy(NodeName, null),
          FromExistingNode;
        false ->
          io:format("Usage: create 'nodeName' (without single quotes)~n"),
          FromExistingNode
      end;

    <<"join">> ->
      if
        FromExistingNode =:= null ->
          io:format("You need to connect to a network!~n"),
          FromExistingNode;

        length(Tokens) =:= 2 ->
          NodeName = lists:nth(2, Tokens),
          deploy(NodeName, FromExistingNode),
          FromExistingNode;

        true ->
          io:format("usage: join 'nodeName' (without single quotes)~n")
      end;

    <<"connect">> ->
      case length(Tokens) =:= 2 of
        true ->
          DestNode = lists:nth(2, Tokens),
          connect(binary_to_atom(DestNode, utf8));
        false ->
          io:format("Usage: connect 'nodeName' (without single quotes)~n"),
          FromExistingNode
      end;

    <<"stop">> ->
      case length(Tokens) == 2 of
        true ->
          DestNode = lists:nth(2, Tokens),
          stop(binary_to_atom(DestNode, utf8)),
          FromExistingNode;
        false ->
          io:format("usage: stop nodeName' (without single quotes)~n"),
          FromExistingNode
      end;

    <<"quit">> -> exit;

    <<"exit">> -> exit;

    <<"">> -> FromExistingNode;

    _ ->
      io:format("unkown command: ~s~n", [lists:nth(1, Tokens)]),
      FromExistingNode
  end.

%%--------------------------------------------------------------------------
%% deploy(Host, Node) -> NextNode
%%        Host = string()
%%        Node = atom() | null
%%        NextNode = atom()
%%
%% Description:
%%   If node is 'null', starts a server in a new databases network.
%%   Otherwise, starts a server that joins an existing network by
%%   specifying one of the active nodes.
%%   Returns the node of the newly created server.
%%--------------------------------------------------------------------------

deploy(NodeName, ExistingNode) ->

  if
    ExistingNode =/= null ->
      CommandLine = io_lib:format("./deploy.sh ~s ~s", [NodeName, ExistingNode]);
    true ->
      CommandLine = io_lib:format("./deploy.sh ~s", [NodeName])
  end,

  % execute the script
  io:format("~s", [os:cmd(CommandLine)]).

%%--------------------------------------------------------------------------
%% connect(FromExistingNode) -> Node | null
%%         FromExistingNode = atom()
%%
%% Description:
%%   Connects to a databases network from one of the server node.
%%   Returns the server node or 'null' if the connection fails.
%%--------------------------------------------------------------------------
connect(FromExistingNode) ->
  ServerPid = rpc:call(FromExistingNode, erlang, whereis, [main_pid]),
  case is_pid(ServerPid) of
    true ->
      io:format("The monitor [~p] is connected to a new netowrk with ", [self()]),
      print_status(FromExistingNode),
      FromExistingNode;
    false ->
      io:format("Monitor [~p] cannot connected to a server~n", [self()])
  end.


%%--------------------------------------------------------------------------
%% stop(ExistingNode) -> ok | error
%%      ExistingNode = atom()
%%
%% Description:
%%   Stops the execution of a running node.
%%   Returns 'ok' if stopped successfully, or 'error' if it failed.
%%--------------------------------------------------------------------------
stop(ExistingNode) ->
  ServerPid = rpc:call(ExistingNode, init, stop, []),
  case ServerPid =:= ok of
    true ->
      io:format("Server [~s] is being stopped~n", [ExistingNode]);
    false ->
      io:format("Server [~s] cannot be stopped~n", [ExistingNode])
  end.

%%--------------------------------------------------------------------------
%% get_server_list(FromExistingNode) -> [pid()]
%%             FromExistingNode = atom()
%%
%% Description:
%%   Returns a PID list of the active servers in the network by
%%   specifying one of the active nodes.
%%--------------------------------------------------------------------------
get_server_list(FromExistingNode) ->
  case FromExistingNode =/= null of
    true ->
      ServerPid = rpc:call(FromExistingNode, erlang, whereis, [main_pid]),
      if
        is_pid(ServerPid)->
          ServerPid ! {get_servers, self()},
          receive
            ServerList -> ServerList
          end;
        true ->
          io:format("Could not connect to node ~s~n", [FromExistingNode]),
          []
      end;
    false ->
      io:format("The monitor [~p] is not connected to a network~n", [self()]),
      []
  end.

print_status(FromExistingNode) ->
  ServerList = get_server_list(FromExistingNode),
  case length(ServerList) > 0 of
    true ->
      io:format("There are ~p active servers:~n", [length(ServerList)]),
      print(ServerList);
    false ->
      none
  end.

print([]) -> ok;
print([ServerPid | OtherServerPid]) ->
  io:format("- ~s~n", [node(ServerPid)]),
  print(OtherServerPid).

