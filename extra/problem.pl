% [1, v, [2, &, 0]] <=> 1 v (2 & 0)

% p([1, v, [2, &, 0]], [0, 1, 0]) <=> T v (F & F) = T

p([A, &, B], L) :- p(A, L), p(B, L).
p([A, v, B], L) :- (p(A, L) ; p(B, L)).
p([~, A], L) :- not(p(A, L)).
p(N, L) :- nth2(L, N, 1).

nth2(0, [X|XS], X).
nth2(N, [_|XS], E) :- N - 1 >= 0, N1 is N - 1, nth2(N1, XS, E).

rev([], B, B).
rev([X|XS], B, R) :- rev(XS, [X|B], R).
rev(L, R) :- rev(L, [], R).

c([1|XS], SS, R) :- c(XS, [[1] | SS], R).
c([0|XS], [S, T | SS], R) :- c(XS, [[[0 | S] | T] | SS], R).
c([0], [S], [0|S]).

cc(X, Y) :- rev(X, R), c(R, [], Y).
