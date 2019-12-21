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
-export([]).

commands(CommandLine, ExistingNode) ->
  Tokens = re:split(string:trim(CommandLine), "\s+"),
  case lists:nth(1, Tokens) of
    <<"status">> ->
      print_status(ExistingNode),
      ExistingNode;

    <<"connect">> ->
      case length(Tokens) == 2 of
        true ->
          DestNode = lists:nth(2, Tokens),
          connect(binary_to_atom(DestNode, utf8));
        false ->
          io:format("Usage: connect <node>~n"),
          ExistingNode
      end;

    <<"add">> ->
      case ExistingNode =:= null of
        true ->
          io:format("The monitor [~p] is connected to a new netowrk with ", [self()]),
          ExistingNode
      end;

    <<"stop">> ->
      case length(Tokens) == 2 of
        true ->
          DestNode = lists:nth(2, Tokens),
          stop(binary_to_atom(DestNode, utf8)),
          ExistingNode;
        false ->
          io:format("Usage: stop <node>~n"),
          ExistingNode
      end;

    <<"quit">> ->  exit;

    <<"">> -> ExistingNode;

    _ ->
      io:format("unkown command: ~s~n", [lists:nth(1, Tokens)]),
      ExistingNode
  end.

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
      print_status(FromExistingNode);
    false ->
      io:format("Monitor [~p] cannot connected to a server", [self()])
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
      io:format("Server [~p] is being stopped", [node(ExistingNode)]),
      ok;
    false ->
      io:format("Server [~p] cannot be stopped", [node(ExistingNode)]),
      error
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

print([]) -> do_nothing;
print([ServerPid | OtherServerPid]) ->
  io:format("- ~s~n", [node(ServerPid)]),
  print(OtherServerPid).

