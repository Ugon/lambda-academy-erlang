%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 10. gru 2015 17:43
%%%-------------------------------------------------------------------
-module(my_module).
-author("Wojciech Pachuta").

%% API
-export([average/1, average2/1, create/1, create_reverse/1]).


average(X) when is_list(X) -> sum(X) / len(X).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

len([_|T]) -> 1 + len(T);
len([]) -> 0.

average2(X) -> average2(X, 0, 0).

average2([H|T], Len, Sum) -> average2(T, Len + 1, Sum + H);
average2([], Len, Sum) -> Sum/Len.

create(X) -> create(X, []).
create(0, Acc) when is_list(Acc) -> Acc;
create(X, Acc) when is_list(Acc) -> create(X - 1, [X | Acc]).

create_reverse(X) -> create_reverse(1, X, []).
create_reverse(X, X, Acc) -> [X|Acc];
create_reverse(Curr, X, Acc) -> create_reverse(Curr + 1, X, [Curr|Acc]).