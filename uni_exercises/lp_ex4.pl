permutation2([], []).
permutation2([A|L], P) :- permutation2(L, P1), app2(S, F, P1), app2(S, [A|F], P).

app2([], L, L).
app2(L, [], L).
app2([X|L1], L2, [X|L3]) :- app2(L1, L2, L3).

not_path(P, E) :- app2(_, [A, B| _], P), not(mem2([A, B], E)).
hamiltonian_path(V, E, C) :- permutation2(V, C), not(not_path(C, E)).

mem2(X, [X|_]).
mem2(X, [_|L1]) :- mem2(X, L1).

hamiltonian_cycle(V, E, [X]) :- hamiltonian_path(V, E, [X]).
hamiltonian_cycle(V, E, [X|L]) :- hamiltonian_path(V, E, [X|L]), app2(_, [Y], L), mem2([Y, X], E).

between2(I, J, I) :- I =< J.
between2(I, J, K) :- I+1 < J, I1 is I + 1, between2(I1, J, K).

generate_colours([], _, []).
generate_colours([X|V1], C, [[X, I] | VC1]) :- between2(1, C + 1, I), generate_colours(V1, C, VC1).

non_correct_colouring(VC, E) :- mem2([U, V], E), mem2([U, C], VC), mem2([V, C], VC).

colour(V, E, C, VC) :- generate_colours(V, C, VC), not(non_correct_colouring(VC, E)).

colour_less_than(V, E, C, VC) :- C1 is C-1, colour(V, E, C1, VC).

length2([], 0).
length2([_|XS], N) :- length2(XS, N1), N is N1+1.

chromatic_number(V, E, C) :- length2(V, V1), between2(1, V1, C), colour(V, E, C, _), not(colour_less_than(V, E, C, _)).
