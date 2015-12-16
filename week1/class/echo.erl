%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 10. gru 2015 18:45
%%%-------------------------------------------------------------------
-module(echo).
-author("Wojciech Pachuta").

%% API
-export([start/0, stop/0, print/1]).

%% internal
-export([init/0]).

start() -> spawn(echo, init, []).

init() ->
  register(echo, self()),
  loop().

loop() ->
  receive
    {print, Msg} -> io:format(Msg), loop();
    stop -> ok
  end.

stop() -> whereis(echo) ! stop.

print(Msg) -> whereis(echo) ! {print, Msg}, ok.