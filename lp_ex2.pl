pow(_, 0, 1).
pow(X, Y, N) :- Y > 0, Y1 is Y-1, pow(X, Y1, N1), N is X*N1.

is_not_pow(X, Y, N) :- pow(X, Y, N1), N1 \= N.

is_pow(X, Y, N) :- not(is_not_pow(X, Y, N)).

mem2(X, [X|_]).
mem2(X, [_|L1]) :- mem2(X, L1).

app2([], L, L).
app2(L, [], L).
app2([X|L1], L2, [X|L3]) :- app2(L1, L2, L3).

permutation([], []).
permutation([A|L], P) :- permutation(L, P1), app2(S, F, P1), app2(S, [A|F], P).

less(A, B) :- A =< B.
check_sort([]).
check_sort([_]).
check_sort([A,B|L]) :- less(A, B), check_sort([B|L]).

sort2(L, P) :- permutation(L, P), check_sort(P).

split([], [], []).
split([A], [A], []).
split([A, B|L], [A|L1], [B|L2]) :- split(L, L1, L2).

merge(L, [], L).
merge([], L, L).
merge([A|L1], [B|L2], [A|L]) :- less(A, B), merge(L1, [B|L2], L).
merge([A|L1], [B|L2], [B|L]) :- not(less(A, B)), merge([A|L1], L2, L).

merge_sort([], []).
merge_sort([A], [A]).
merge_sort(L, P) :- split(L, P1, P2), merge_sort(P1, L1), merge_sort(P2, L2), merge(L1, L2, P).

filter([], _, []).
filter([A|L], N, [A|P]) :- less(A, N), (L, N, P).
filter([A|L], N, P) :- not(less(A, N)), filter(L, N, P).

add_one(N, N1) :- N1 is N+1.

map([], _, []).
map([H|T], F, [H1|T1]) :- call(F, H, H1), map(T, F, T1).

quick_sort([A|L], P) :- partition(A, L, P1, P2), quick_sort(P1, P3), quick_sort(P2, P4), app2(P3, [A|P4], P).
quick_sort([], []).

partition(_, [], [], []).
partition(H, [H1|T], [H1|P1], P2) :- less(H1, H), partition(H, T, P1, P2).
partition(H, [H1|T], P1, [H1|P2]) :- not(less(H1, H)), partition(H, T, P1, P2).
