% ex. 1

% a0 = 0; an = 2*(a(n - 1)) + n
% b0 = 1; bn = 3*(b(n -1)) + n^2 - 1

genA(0, 0).
genA(N, E) :- N - 1 >= 0, N1 is N - 1, genA(N1, E1), E is 2 * E1 + N.

genB(0, 1).
genB(N, E) :- N - 1 >= 0, N1 is N - 1, genB(N1, E1), E is 3 * E1 + N**2 - 1.

nat(0).
nat(N) :- nat(N1), N is N1 + 1.

between2(I, J, I) :- I =< J.
between2(I, J, K) :- I + 1 =< J, I1 is I + 1, between2(I1, J, K).

gen_pairs(A, B) :- nat(N), between2(0, N, B), A is N - B.

pairs_of_a_and_b(N) :- gen_pairs(A, B), genA(A, AI), genB(B, BJ), N is AI + BJ.

% ex. 2
is_cyclic(E) :- hamiltonian_cycle(E), oyler_cycle(E).

app2([], L, L).
app2(L, [], L).
app2([X | XS], YS, [X | ZS]) :- app2(XS, YS, ZS).

permutation2([], []).
permutation2([X | XS], P) :- permutation2(XS, P1), app2(S, F, P1), app2(S, [X | F], P).

mem2(X, [X|_]).
mem2(X, [_|XS]) :- mem2(X, XS).

gen_vertices([], []).
gen_vertices([[X, Y] | E], [X, Y | V]) :- gen_vertices(E, V), not(mem2(X, V)), not(mem2(Y, V)).
gen_vertices([[X, Y] | E], [Y | V]) :- gen_vertices(E, V), mem2(X, V), not(mem2(Y, V)).
gen_vertices([[X, Y] | E], [X | V]) :- gen_vertices(E, V), not(mem2(X, V)), mem2(Y, V).
gen_vertices([[X, Y] | E], V) :- gen_vertices(E, V), mem2(X, V), mem2(Y, V).

not_hamiltonian_path(E, P) :- app2(_, [X, Y | _], P), not(mem2([X, Y], E)).

hamiltonian_path(E, P) :- gen_vertices(E, V), permutation2(V, P), not(not_hamiltonian_path(E, P)).

hamiltonian_cycle(E) :- hamiltonian_path(E, P), app2([X], _, P), app2(_, [Y], P), mem2([Y, X], E).

not_oyler_path(P) :- app2(_, [[X, Y], [Z, T] | _], P), Y \= Z.
oyler_path(E, P) :- permutation2(E, P), not(not_oyler_path(P)).

oyler_cycle(E) :- oyler_path(E, P), app2([[X, _]], _, P), app2(_, [[_, Y]], P), X == Y.

split([], [], []).
split([X|XS], [X|F], S) :- split(XS, F, S).
split([X|XS], F, [X|S]) :- split(XS, F, S).

more_than_one_common_element(XS, YS) :- mem2(X, XS), mem2(Y, XS), X \= Y, mem2(X, YS), mem2(Y, YS).
not_zero_common_elements(XS, YS) :- mem2(X, XS), mem2(Y, YS).

one_common_element(XS, YS) :- not_zero_common_elements(XS, YS), not(more_than_one_common_element(XS, YS)).

is_eight_graph(E) :- split(E, F, S), gen_vertices(F, V1), gen_vertices(S, V2), one_common_element(V1, V2).
