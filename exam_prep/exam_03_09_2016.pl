% ex. 1
% Predicate p(L, N) that states - there is N elements from L - a1...an, such that for every N - 1 elements b1...bn-1,
% gcd(a1...an) ≠ gcd(b1...bn-1)

gcd(X, X, X).
gcd(X, Y, Z) :- X > Y, X1 is X - Y, gcd(X1, Y, Z), !.
gcd(X, Y, Z) :- X < Y, gcd(Y, X, Z), !.

list_gcd([X, Y | L], G) :- gcd(X, Y, T), list_gcd([T|L], G), !.
list_gcd([X, Y], G) :- gcd(X, Y, G), !.

subset([], []).
subset([A|X], [A|S]) :- subset(X, S).
subset([_|X], S) :- subset(X, S).

length2([], 0).
length2([_|L], N) :- length2(L, N1), N is N1 + 1.

subset_with_N_elements(L, N, S) :- subset(L, S), length2(S, N).
 
subset_with_less_elements_and_different_gcd(L, N) :- N1 is N - 1,
                                                     subset_with_N_elements(L, N1, S),
                                                     list_gcd(S, G1),
                                                     list_gcd(L, G),
                                                     G == G1.

p(L, N) :- subset_with_N_elements(L, N, S), not(subset_with_less_elements_and_different_gcd(S, N)).

% ex. 2
% A is devoured by B when (∀x∀y ∈ A)(x + y ∈ B)

app2([], L, L).
app2(L, [], L).
app2([A|L1], L2, [A|L3]) :- app2(L1, L2, L3).

mem2(X, L) :- app2(_, [X|_], L).

a_has_sum_not_in_b(A, B) :- mem2(X, A), mem2(Y, A), S is X + Y, not(mem2(S, B)), !.

devoured(A, B) :- not(a_has_sum_not_in_b(A, B)).

some_next_element_is_not_devoured(L) :- subset_with_N_elements(L, 2, [A, B]), not(devoured(A, B)).
all_devoured(S) :- not(some_next_element_is_not_devoured(S)).

not_longest_all_devoured(L, S) :- subset(L, T), length2(S, N1), length(T, N), N > N1, all_devoured(T).
longest_all_devoured(L, S) :- subset(L, S), all_devoured(S), not(not_longest_all_devoured(L, S)).
