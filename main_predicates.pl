app2([], L, L).
app2(L, [], L).
app2([X|L1], L2, [X|L3]) :- app2(L1, L2, L3).

mem2(X, [X|_]).
mem2(X, [_|L]) :- mem2(X, L).

length2([], 0).
length2([_|L], N) :- length2(L, N1), N is N1 + 1.

permutation2([], []).
permutation2([A|L], P) :- permutation2(L, P1), app2(S, F, P1), app2(S, [A|F], P).

between2(I, J, I) :- I =< J.
between2(I, J, K) :- I + 1 =< J, I1 is I + 1, between2(I1, J, K).

% find if there's path between two nodes in a graph
not_edge(V, E, A, B) :- mem2(A, V), mem2(B, V), not(mem2([A, B], E)).
edge(V, E, A, B) :- not(not_path(V, E, A, B)).

path(E, A, B) :- walk(E, A, B, []).
walk(E, A, B, V) :- mem2([A, X], E),
                    not(mem2(X, V)),
                    (
                      X == B;
                      walk(E, X, B, [X|V])
                    ).

subset([], []).
subset([A|L1], [A|S]) :- subset(L1, S).
subset([_|L1], S) :- subset(L1, S).
