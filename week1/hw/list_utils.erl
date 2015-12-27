%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(list_utils).
-author("Wojciech Pachuta").

%% API
-export([delete/2, keyfind/3, reverse/1, filter/2, map/2, keydelete/3, keyreplace/4, keystore/4]).

map(List, Function) -> [Function(X) || X <- List].

filter(List, Predicate) -> [X || X <- List, Predicate(X)].

reverse(List) -> reverse_impl(List, []).

reverse_impl([], Acc) -> Acc;
reverse_impl([H | T], Acc) -> reverse_impl(T, [H | Acc]).

delete(Elem, List) -> delete_forward(Elem, List, []).

delete_forward(_, [], Temp) -> rewind([], Temp);
delete_forward(Elem, [Elem | T], Temp) -> rewind(T, Temp);
delete_forward(Elem, [H | T], Temp) -> delete_forward(Elem, T, [H | Temp]).

rewind(List, []) -> List;
rewind(List, [H | T]) -> rewind([H | List], T).

keydelete(Key, N, TupleList) -> keydelete_forward(Key, N, TupleList, []).

keydelete_forward(_, _, [], Temp) -> keydelete_back([], Temp);
keydelete_forward(Key, N, [H | T], Temp) when element(N, H) == Key -> keydelete_back(T, Temp);
keydelete_forward(Key, N, [H | T], Temp) -> keydelete_forward(Key, N, T, [H | Temp]).

keydelete_back(List, []) -> List;
keydelete_back(List, [H | T]) -> keydelete_back([H | List], T).

keyfind(_, _, [])  -> false;
keyfind(Key, N, [H | _]) when element(N, H) == Key -> H;
keyfind(Key, N, [_ | T]) -> keyfind(Key, N, T).

keyreplace(Key, N, TupleList, NewTuple) -> keyreplace_forward(Key, N, TupleList, NewTuple, []).

keyreplace_forward(_, _, [], _, Temp) -> rewind([], Temp);
keyreplace_forward(Key, N, [H | T], NewTuple, Temp) when element(N, H) == Key -> rewind([NewTuple | T], Temp);
keyreplace_forward(Key, N, [H | T], NewTuple, Temp) -> keyreplace_forward(Key, N, T, NewTuple, [H | Temp]).

keystore(Key, N, TupleList, NewTuple) -> keystore_forward(Key, N, TupleList, NewTuple, []).

keystore_forward(_, _, [], NewTuple, Temp) -> rewind([NewTuple], Temp);
keystore_forward(Key, N, [H | T], NewTuple, Temp) when element(N, H) == Key -> rewind([NewTuple | T], Temp);
keystore_forward(Key, N, [H | T], NewTuple, Temp) -> keystore_forward(Key, N, T, NewTuple, [H | Temp]).

