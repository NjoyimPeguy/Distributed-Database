%%%-------------------------------------------------------------------
%%% @author Peguy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2019 13:56
%%%-------------------------------------------------------------------

-module(oxygen_db).
-author("Peguy").

%% API
-compile(export_all).

new() -> maps:new().

read(Key, Db) -> maps:get(Key, Db, none).

write(Key, Value, Db) -> Db#{Key => Value}.

to_list(Db) -> maps:to_list(Db).
