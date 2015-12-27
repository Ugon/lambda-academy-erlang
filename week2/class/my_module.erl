%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(my_module).
-author("Wojciech Pachuta").

%% API
-export([sum/1, print_even/1]).

sum(List) -> lists:foldl(fun(X, Y) -> X + Y end, 0, List).

print_even(List) ->
  Even = lists:filter(fun(X) -> X rem 2 == 0 end, List),
  lists:foreach(fun(X) -> io:format("~p~n", [X]) end, Even).