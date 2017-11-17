% for every X from L, every Y from L exists Z from L, (exists a from X ( a is from Y => a is from Z ))

p(L) :- not((
  member(X, L), member(Y, L),
  not((
    member(Z, L),
    not((member(A, X), member(A, Y), not(member(A, Z))))
  ))
)).

gcd(0, B, B).
gcd(A, B, C) :- A > 0, X is mod(B, A), gcd(X, A, C).

% няма взаимно прости елементи
base(L) :- not((member(A, L), member(B, L), A \= B,
    gcd(A, B, X), X > 1)).

sub([], []).
sub([_|XS], R) :- sub(XS, R).
sub([X|XS], [X|R]) :- sub(XS, R).

gen(M, L) :- sub(M, L), base(L).

superset(L, S) :- not((member(A, L),
  not(member(A, S)))).

max(M, L) :- gen(M, L), not((
  gen(M, S), L \= S, superset(L, S))).
