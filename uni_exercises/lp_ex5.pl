natural(0).
natural(N) :- natural(N1), N is N1 + 1.

map_to_pair(N, 0, K) :- N mod 2 =:= 1, K is (N - 1) / 2.
map_to_pair(N, A, B) :- N mod 2 =:= 0, N1 is N/2, map_to_pair(N1, A1, B), A is A1 + 1.

gen_pair(A, B) :- natural(N), N1 is N + 1, map_to_pair(N1, A, B).

map_to_finite_set(0, I, []).
map_to_finite_set(N, I, [I | F]) :- N mod 2 =:= 1, I1 is I + 1, N1 is (N-1)/2, map_to_finite_set(N1, I1, F).
map_to_finite_set(N, I, F) :- N =/= 0, N mod 2 =:= 0, I1 is I + 1, N1 is N/2, map_to_finite_set(N1, I1, F).

gen_finite_set(F) :- natural(N), map_to_finite_set(N, 0, F).
