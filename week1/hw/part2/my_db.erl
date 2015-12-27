%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(my_db).
-author("Wojciech Pachuta").

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).

%% Server callbacks
-export([init/0, loop/1]).

start() -> spawn(?MODULE, init, []), ok.

stop() -> whereis(?SERVER) ! stop, ok.

write(Key, Element) -> whereis(?SERVER) ! {write, {Key, Element}}, ok.

delete(Key) -> whereis(?SERVER) ! {delete, Key}, ok.

read(Key) ->
  whereis(?SERVER) ! {read, {Key, self()}},
  receive
    Res -> Res
  end.

match(Element) ->
  whereis(?SERVER) ! {match, {Element, self()}},
  receive
    Res -> Res
  end.



init() ->
  register(?SERVER, self()),
  loop(db:new()).

loop(Db) ->
  receive
    {write, {Key, Element}} -> loop(db:write(Key, Element, Db));
    {delete, Key} -> loop(db:delete(Key, Db));
    {read, {Key, Pid}} -> Pid ! db:read(Key, Db), loop(Db);
    {match, {Element, Pid}} -> Pid ! db:match(Element, Db), loop(Db);
    stop -> ok
  end.

