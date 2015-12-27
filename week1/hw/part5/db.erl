%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(db).
-author("Wojciech Pachuta").

%% API
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

-record(db_entry, {key, value}).

new() -> [].

destroy(_) -> ok.

write(Key, Element, DbRef) -> list_utils:keystore(Key, #db_entry.key, DbRef, #db_entry{key = Key, value = Element}).

delete(Key, DbRef) -> list_utils:keydelete(Key, #db_entry.key, DbRef).

read(Key, DbRef) ->
  case list_utils:keyfind(Key, #db_entry.key, DbRef) of
    false -> {error, instace};
    DbEntry -> {ok, DbEntry#db_entry.value}
  end.

match(Element, DbRef) ->
  Filtered = list_utils:filter(DbRef, fun(X) -> X#db_entry.value == Element end),
  list_utils:map(Filtered, fun(X) -> X#db_entry.key end).
