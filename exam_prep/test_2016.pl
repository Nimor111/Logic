app2(L, [], L).
app2([], L, L).
app2([A|L1], L2, [A|L3]) :- app2(L1, L2, L3).

mem2(X, [X|_]).
mem2(X, [_|L]) :- mem2(X, L).
 
not_prefix(L, L1) :- not(app2(L, _, L1)).

prefix(L, L1) :- not(not_prefix(L, L1)).

not_prefix_list(L) :- app2(_, [A, B| _], L), not_prefix(A, B).

prefix_list(L) :- not(not_prefix_list(L)).

gen_list(I, I, [I]).
gen_list(I, J, L) :- J-1 >= I, J1 is J - 1, gen_list(I, J1, L1), app2([J], L1, L).

gen_subset([], []).
gen_subset([A | L], [A | F]) :- gen_subset(L, F).
gen_subset([A | L], F) :- gen_subset(L, F).

powerset(Set, Powerset) :- setof(Subset, gen_subset(Set, Subset), Powerset).

all_prefix_lists(L) :- gen_list(1, 4, L1), reverse(L1, L2), gen_subset(L2, L), prefix_list(L).

between2(I, J, I) :- I =< J.
between2(I, J, K) :- I + 1 =< J, I1 is I + 1, between2(I1, J, K).

generate_colouring([], _, []).
generate_colouring([X | V], C, [[X, I] | VC1]) :- between2(1, C, I), generate_colouring(V, C, VC1).

generate_vertices([], []).
generate_vertices([[X, Y] | E], [X, Y|V]) :- generate_vertices(E, V).

dedup([], []).
dedup([A| V], S) :- dedup(V, S), mem2(A, S).
dedup([A | V], [A | S]) :- dedup(V, S), not(mem2(A, S)).

gen_colouring_from_edges(E, C, VC) :- generate_vertices(E, V1), dedup(V1, V), generate_colouring(V, C, VC).

non_correct_colouring(E, VC) :- mem2([U, X], E),
                                mem2([X, V], E),
                                mem2([V, U], E),
                                mem2([U, C], VC),
                                mem2([X, C], VC),
                                mem2([V, C], VC).

colouring(E, VC) :- gen_colouring_from_edges(E, 2, VC), not(non_correct_colouring(E, VC)).
