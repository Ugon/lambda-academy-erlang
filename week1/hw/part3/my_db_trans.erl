%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(my_db_trans).
-author("Wojciech Pachuta").

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0, write/2, delete/1, read/1, match/1, lock/0, unlock/0]).

%% Server callbacks
-export([init/0, loop/1]).

-record(state, {db, locking_pid}).

start() -> spawn(?MODULE, init, []), ok.

stop() -> ?SERVER ! stop, ok.

write(Key, Element) -> ?SERVER ! {write, self(), {Key, Element}}, return_received().

delete(Key) -> ?SERVER ! {delete, self(), Key}, return_received().

read(Key) -> ?SERVER ! {read, self(), Key}, return_received().

match(Element) -> ?SERVER ! {match, self(), Element}, return_received().

lock() -> ?SERVER ! {lock, self()}, return_received().

unlock() -> ?SERVER ! {unlock, self()}, return_received().


init() ->
  register(?SERVER, self()),
  loop(#state{db = db:new(), locking_pid = none}).

loop(#state{db = Db, locking_pid = LockingPid} = State) ->
  receive
    {lock, Pid} when LockingPid == none -> Pid ! ok, loop(State#state{locking_pid = Pid});
    {unlock, LockingPid} -> LockingPid ! ok, loop(State#state{locking_pid = none});

    {_, Pid, _} when Pid /= LockingPid -> Pid ! {error, notrans}, loop(State);

    {write, Pid, {Key, Element}} -> Pid ! ok, loop(State#state{db = db:write(Key, Element, Db)});
    {delete, Pid, Key} -> Pid ! ok, loop(State#state{db = db:delete(Key, Db)});
    {read, Pid, Key} -> Pid ! db:read(Key, Db), loop(State);
    {match, Pid, Element} -> Pid ! db:match(Element, Db), loop(State);

    stop -> ok
  end.


return_received() ->
  receive
    Ans -> Ans
  end.
