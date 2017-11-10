permutation2([], []).
permutation2([A|L], P) :- permutation2(L, P1), app2(S, F, P1), app2(S, [A|F], P).

mod2(X, Y, X) :- X < Y.
mod2(X, Y, E) :- X1 is X-Y, X1 >= 0, mod2(X1, Y, E).

mem2(X, [X|_]).
mem2(X, [_|L1]) :- mem2(X, L1).

app2([], L, L).
app2(L, [], L).
app2([X|L1], L2, [X|L3]) :- app2(L1, L2, L3).

bin(0, [0]).
bin(1, [1 | []]).
bin(X, [X1 | B]) :- X > 1, mod2(X, 2, X1), X2 is X div 2, bin(X2, B).

len([], 0).
len([_|XS], N) :- len(XS, N1), N is N1 + 1.

fill_with_zeros([], _, []).
fill_with_zeros(L, N, L) :- N >= 0, len(L, N1), N == N1.
fill_with_zeros(L, N, [0 | L1]) :- N - 1 >= 0, N1 is N - 1, fill_with_zeros(L, N1, L1).

gen_vertices(N, CN, [B1]) :- N == 0, bin(N, B), fill_with_zeros(B, CN, B1).
gen_vertices(N, CN, L) :- pow(2, N, N1), generator(N1, CN, L).

generator(N, _, []) :- N == 0.
generator(N, CN, [N4 | L]) :- N-1 >= 0,
			   N1 is N-1,
			   bin(N1, N2),
			   reverse(N2, N3),
			   fill_with_zeros(N3, CN, N4),
			   generator(N1, CN, L).

bit_differences([], [], 0).
bit_differences([X | XS], [Y | YS], N) :- X \= Y, bit_differences(XS, YS, N1), N is N1 + 1.
bit_differences([X | XS], [Y | YS], N) :- X == Y, bit_differences(XS, YS, N).

generate_edge(L, [A, B]) :- app2(_, [A, B|_], L),
		            bit_differences(A, B, D),
			    D == 1.

pair2(L, [X, Y]) :- append([_, [X], _, [Y], _], L).

gen_hypercube_edge(N, V, [X, Y]) :- gen_vertices(N, N, V), pair2(V, [X, Y]), bit_differences(X, Y, D), D == 1.

gen_hypercube(N, V, E) :- gen_vertices(N, N, V), gen_hypercube_edge(N, V, E).
