% ex 1
c(0, []).
c(N, [X|XS]) :- N > 0, M is N - 1, between(0, 1, X),
    c(M, XS).

genKS(1, S, [S]).
genKS(K, S, [X|XS]) :- K > 1, M is K - 1, between(0, S, X), T is S - X, genKS(M, T, XS).

t([], []).
t([X|XS], [L|LS]) :- c(X, L), t(XS, LS).

nat(0).
nat(X) :- nat(Y), X is Y + 1.

p(L) :- nat(N), between(1, N, K), S is N - K, genKS(K, S, T), t(T, B),
  s(B, L).

s([X], B, [T]) :- append(B, X, T).
s([X|XS], B, [T|TS]) :- append(B, X, T), s(XS, T, TS).
s(L, M) :- s(L, [], M).
% ex 1

% ex 2
getV([], []).
getV([[A, B] | XS], [A, B | Res]) :- getV(XS, Res), not(member(A, Res)), not(member(B, Res)).
getV([[A, B] | XS], [A|Res]) :- getV(XS, Res), not(member(A, Res)), member(B, Res).
getV([[A, B] | XS], [B|Res]) :- getV(XS, Res), not(member(B, Res)), member(A, Res).
getV([[A, B] | XS], Res) :- getV(XS, Res), member(B, Res), member(A, Res).

split([], [], []).
split([X | XS], [X | L], R) :- split(XS, L, R).
split([X | XS], L, [X | R]) :- split(XS, L, R).

c(V, E) :- not((
  member(A, V), member(B, V), member(C, V),
  member([A, B], E), member([B, C], E), member([C, A], E)
)).

p(G) : getV(G, V), split(V, L, R), c(L, E), c(R, E).

% ex 2
