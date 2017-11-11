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

all_prefix_lists(L) :- gen_list(1, 4, L1), reverse(L1, L2), powerset(L2, P), gen_subset(P, L), prefix_list(L).
