% ex.1
% L - list, p(L) <=> ∀x∀y∃z∃p(same_elements(x, y, p), contains(z, p))

mem2(X, [X|_]).
mem2(X, [_|XS]) :- mem2(X, XS).

same_elements(_, [], []).
same_elements([], _, []).
same_elements([X|XS], YS, [X|P]) :- mem2(X, YS), same_elements(XS, YS, P).
same_elements([X|XS], YS, P) :- not(mem2(X, YS)), same_elements(XS, YS, P).

not_contains([], _).
not_contains(XS, YS) :- mem2(X, XS), not(mem2(X, YS)).

contains(XS, YS) :- not(not_contains(XS, YS)).

contains_elements(L, P) :- mem2(Z, L), contains(P, Z).

not_p(L) :- mem2(X, L), mem2(Y, L), same_elements(X, Y, P),
            not(contains_elements(L, P)).

p(L) :- not(not_p(L)).

% ex. 2
 
between2(I, J, I) :- I =< J.
between2(I, J, K) :- I + 1 =< J, I1 is I + 1, between2(I1, J, K).

divisor_different_than_one(I, J) :- between2(2, I, K), mod(I, K) =:= 0, mod(J, K) =:= 0.

coprime(I, J) :- not(divisor_different_than_one(I, J)).

not_base(L) :- mem2(X, L), mem2(Y, L), not(X == Y), not(coprime(X, Y)).
base(L) :- not(not_base(L)).

subset([], []).
subset([X|XS], [X|L]) :- subset(XS, L).
subset([_|XS], L) :- subset(XS, L).

gen(M, L) :- subset(M, L), base(L).

not_superset(M, L) :- mem2(X, M), not(mem2(X, L)).  

has_base_superset(M, L) :- gen(M, N), L \= N, superset(L, N).
max(M, L) :- gen(M, L), not(has_base_superset(M, L)).
