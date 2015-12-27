%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(db).
-author("Wojciech Pachuta").

%% API
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

-define(TABLE, ?MODULE).

-record(db_entry, {key, value}).

new() -> ets:new(?TABLE, [set, named_table, private, {keypos, #db_entry.key}]).

destroy(DbRef) -> ets:delete(DbRef), ok.

write(Key, Element, DbRef) -> ets:insert(DbRef, #db_entry{key = Key, value = Element}), DbRef.

delete(Key, DbRef) -> ets:delete(DbRef, Key), DbRef.

read(Key, DbRef) ->
  case ets:lookup(DbRef, Key) of
    [Res] -> {ok, Res#db_entry.value};
    [] -> {error, instace}
  end.

match(Element, DbRef) -> lists:flatten(ets:match(DbRef, #db_entry{key = '$1', value = Element})).
