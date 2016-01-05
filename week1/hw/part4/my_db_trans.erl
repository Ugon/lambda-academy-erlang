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

%%server will unlock transaction when client dies
%%its up to client to trap signals caused by server failure during transaction (between lock and unlock calls)
lock() ->
  ServerPid = whereis(?SERVER),
  link(ServerPid),
  ServerPid ! {lock, self()},
  return_received().

unlock() ->
  ServerPid = whereis(?SERVER),
  ServerPid ! {unlock, self()},
  unlink(ServerPid),
  return_received().


init() ->
  register(?SERVER, self()),
  process_flag(trap_exit, true),
  loop(#state{db = db:new(), locking_pid = none}).

loop(#state{db = Db, locking_pid = LockingPid} = State) ->
  receive
    {'EXIT', LockingPid, _} -> loop(State#state{locking_pid = none});

    {lock, Pid} when LockingPid == none -> Pid ! ok, loop(State#state{locking_pid = Pid});
    {lock, OtherPid} when OtherPid /= LockingPid -> OtherPid ! {error, server_locked}, loop(State);
    {lock, LockingPid} -> LockingPid ! {error, already_locked}, loop(State);

    {unlock, LockingPid} -> LockingPid ! ok, loop(State#state{locking_pid = none});
    {unlock, Pid} when Pid /= LockingPid -> Pid ! {error, notrans}, loop(State);

    {write, LockingPid, {Key, Element}} -> LockingPid ! ok, loop(State#state{db = db:write(Key, Element, Db)});
    {delete, LockingPid, Key} -> LockingPid ! ok, loop(State#state{db = db:delete(Key, Db)});
    {read, LockingPid, Key} -> LockingPid ! db:read(Key, Db), loop(State);
    {match, LockingPid, Element} -> LockingPid ! db:match(Element, Db), loop(State);

    {_Command, NonLockingPid, _} -> NonLockingPid ! {error, notrans}, loop(State);

    stop -> ok
  end.


return_received() ->
  receive
    Ans -> Ans
  end.
