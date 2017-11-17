% 03.09.2016 ex 2
p(A, B) :- not((member(X, A), member(Y, A), T is X + Y,
           not(member(T, B)))).

psub([], []).
psub([X|XS], R) :- psub(XS, R).
psub([X|XS], T) :- psub(XS, R), append(A, B, R), append(A, [X|B], T).

c(L, S) :- psub(L, S),  not((append(_, [X|XS], S), member(X, XS))),
      not((append(_, [Y|YS], S), member(Z, YS), not(p(Y, Z)))).

q(L, S) :- c(L, S), length(S, N),
      not((c(L, P), length(P, M), M > N)).
