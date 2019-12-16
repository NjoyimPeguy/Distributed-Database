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
-export([connect/1]).

connect(ExistingNode) ->
  AliveServer = rpc:call(ExistingNode, erlang, whereis, [main_pid]),
  sendRequests(AliveServer).

sendRequests(AliveServer) ->

  % Taking the client's query.
  Input = io:get_line("Query> "),
  % Converting the client's query to lower cases.
  Input_in_lower_case = string:to_lower(Input),
  % Splitting the client's query.
  Query = re:split(string:trim(Input_in_lower_case), "\s+"),

  % Checking each argument
  case lists:nth(1, Query) of
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
                  sendRequests(AliveServer);
                false ->
                  io:format("SERVER [~p] : sorry, I could not find your data!~n", [AliveServer]),
                  sendRequests(AliveServer)
              end
          end;
        false ->
          io:format("Query usage: read 'key'~n"),
          sendRequests(AliveServer)
      end;

    <<"write">> ->
      case length(Query) == 2  of
        true ->
          Value = binary_to_list(lists:nth(2, Query)),
          AliveServer ! {write , self() , Value},
          receive
            {write_successful, Key} ->
              io:format("CLIENT [~p] : has just received the Key '~s' from SERVER [~p] after writting the data '~s'~n", [self(), Key, AliveServer, Value]),
              sendRequests(AliveServer)
          end;
        false ->
          io:format("Query usage: write 'value'~n"),
          sendRequests(AliveServer)
      end;

    <<"quit">> ->
      io:format("CLIENT [~p] stops requesting data to the SERVER [~p] for now",  [self(), AliveServer]),
      init:stop();

    <<"">> ->
      io:format("Query usage: write 'value'~n"),
      io:format("Query usage: read 'key'~n"),
      sendRequests(AliveServer)
  end.



