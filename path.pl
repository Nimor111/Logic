app2(L, [], L).
app2([], L, L).
app2([X|L1], L2, [X|L3]) :- app2(L1, L2, L3).

mem2(X, [X|_]).
mem2(X, [_|L]) :- mem2(X, L).

permutation2([], []).
permutation2([H | T], P) :- permutation2(T, P1), app2(S, F, P1), app2(S, [H|F], P).

pair2(L, [X, Y]) :- append([_, [X], _, [Y], _], L).

path(V, E, U, W) :- mem2(U, V), mem2(W, V), walk(E, U, W, []).

walk(E, U, W, V) :- mem2([U, X], E),
                    not(mem2(X, V)),
                    (
                      X==W;
                      walk(E, X, W, [X|V])
                    ).

not_connected(V, E) :- app2(_, [U, W|_], V), not(path(V, E, U, W)).
connected(V, E) :- not(not_connected(V, E)).

gen_path(E, U, W, V, [U, W]) :- mem2([U, W], E), not(mem2(W, V)).
gen_path(E, U, W, V, [U | P]) :- mem2([U, X], E), not(mem2(X, V)), gen_path(E, X, W, [X | V], P).

gen_weight_of_paths(E, U, W, V, C) :- mem2([U, W, C], E).
gen_weight_of_paths(E, U, W, V, P) :- mem2([U, X, C], E),
                                      not(mem2(X, V)),
                                      gen_weight_of_paths(E, X, W, [X | V], P1),
                                      P is P1 + C.

not_shortest_path(E, U, W, V, P) :- gen_weight_of_paths(E, U, W, V, C), C < P.

shortest_path(E, U, W, V, P) :- gen_weight_of_paths(E, U, W, V, P), not(not_shortest_path(E, U, W, V, P)).
