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

stop() -> whereis(?SERVER) ! stop, ok.

write(Key, Element) -> whereis(?SERVER) ! {write, self(), {Key, Element}}, return_received().

delete(Key) -> whereis(?SERVER) ! {delete, self(), Key}, return_received().

read(Key) -> whereis(?SERVER) ! {read, self(), Key}, return_received().

match(Element) -> whereis(?SERVER) ! {match, self(), Element}, return_received().

lock() -> whereis(?SERVER) ! {lock, self()}, return_received().

unlock() -> whereis(?SERVER) ! {unlock, self()}, return_received().


init() ->
  register(?SERVER, self()),
  loop(#state{db = db:new(), locking_pid = none}).

loop(State) ->
  receive
    {lock, Pid} when State#state.locking_pid == none -> Pid ! ok, loop(State#state{locking_pid = Pid});
    {unlock, Pid} when State#state.locking_pid == Pid -> Pid ! ok, loop(State#state{locking_pid = none});

    Tuple when State#state.locking_pid /= element(2, Tuple) ->
      element(2, Tuple) ! {error, notrans}, loop(State);

    {write, Pid, {Key, Element}} -> Pid ! ok, loop(State#state{db = db:write(Key, Element, State#state.db)});
    {delete, Pid, Key} -> Pid ! ok, loop(State#state{db = db:delete(Key, State#state.db)});
    {read, Pid, Key} -> Pid ! db:read(Key, State#state.db), loop(State);
    {match, Pid, Element} -> Pid ! db:match(Element, State#state.db), loop(State);

    stop -> ok
  end.


return_received() ->
  receive
    Ans -> Ans
  end.
