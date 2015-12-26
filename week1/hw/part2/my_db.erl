%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 16. gru 2015 17:38
%%%-------------------------------------------------------------------
-module(my_db).
-author("Wojciech Pachuta").

%% API
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).

%% Server callbacks
-export([init/0, loop/1]).

start() -> spawn(my_db, init, []), ok.

stop() -> whereis(my_db) ! stop, ok.

write(Key, Element) -> whereis(my_db) ! {write, {Key, Element}}, ok.

delete(Key) -> whereis(my_db) ! {delete, Key}, ok.

read(Key) ->
  whereis(my_db) ! {read, {Key, self()}},
  receive
    Res -> Res
  end.

match(Element) ->
  whereis(my_db) ! {match, {Element, self()}},
  receive
    Res -> Res
  end.



init() ->
register(my_db, self()),
loop(db:new()).

loop(Db) ->
receive
  {write, {Key, Element}} -> loop(db:write(Key, Element, Db));
  {delete, Key} -> loop(db:delete(Key, Db));
  {read, {Key, Pid}} -> Pid ! db:read(Key, Db), loop(Db);
  {match, {Element, Pid}} -> Pid ! db:match(Element, Db), loop(Db);
  stop -> ok
end.

