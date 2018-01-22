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

split([], [], []).
split([A|XS], [A|YS], ZS) :- split(XS, YS, ZS).
split([A|XS], YS, [A|ZS]) :- split(XS, YS, ZS).

cartesian_product1(_, [], []).
cartesian_product1(A, [X|XS], [[A, X] | YS]) :- cartesian_product1(A, XS, YS).

cartesian_product([], _, []).
cartesian_product([A|XS], B, CP) :- cartesian_product1(A, B, C), cartesian_product(XS, B, C1), app2(C, C1, CP), !.

cartesian_square(A, C) :- cartesian_product(A, A, C).

natural(0).
natural(N) :- natural(N1), N is N1 + 1.

gen_pairs(A, B) :- natural(N), between2(0, N, B), A is N - B.

divisors_other_than_1(0).
divisors_other_than_1(1).
divisors_other_than_1(A) :- AS is round(sqrt(A)), between(2, AS, B), A mod B =:= 0.

prime(A) :- not(divisors_other_than_1(A)).
  
% find if there's path between two nodes in a graph
edge(E, A, B) :- mem2([A, B], E).

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
