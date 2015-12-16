%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 16. gru 2015 17:38
%%%-------------------------------------------------------------------
-module(db).
-author("Wojciech Pachuta").

%% API
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).


new() -> [].

destroy(_) -> ok.

write(Key, Element, DbRef) -> list_utils:keystore(Key, 1, DbRef, {Key, Element}).

delete(Key, DbRef) -> list_utils:keydelete(Key, 1, DbRef).

read(Key, DbRef) ->
  case list_utils:keyfind(Key, 1, DbRef) of
    false -> {error, instace};
    Tuple -> {ok, element(2, Tuple)}
  end.

match(Element, DbRef) ->
  Filtered = list_utils:filter(DbRef, fun(X) -> element(2, X) == Element end),
  list_utils:map(Filtered, fun(X) -> element(1, X) end).
