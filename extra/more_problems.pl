% rt term
% [] is an rt term
% if A and B are rt terms, then [A, [A, B]] is an rt term

r([], 1).
r([A, B], N) :- N > 1, M is N - 1, between(1, M, RA), RB is N - RA, r(A, RA), r(B, RB).

nat(0).
nat(X) :- nat(X1), X is X1 + 1.

p(L) :- nat(N), r(L, N).

% 13.06.2012
injection(F) :- not((member([A, B], F), member([C, B], F), A \= C)).
increasing(F) :- not((member([A, FA], F), member([B, FB], F), A < B, FA > FB)).
decreasing(F) :- not((member([A, FA], F), member([B, FB], F), A < B, FA < FB)).

monotonous(F) :- increasing(F); decreasing(F).

composition1(_, [], []).
composition1([X, Y], [[Y, Z] | YS], [[X, Z] | RS]) :- composition1([X, Y], YS, RS).
composition1([X, Y], [[U, Z] | YS], RS) :- Y \= U, composition1([X, Y], YS, RS).

composition([], _, []).
composition([[X, Y] | XS], F2, T) :- composition1([X, Y], F2, A), composition(XS, F2, B), append(A, B, T).

in_circle(X, Y, R, A, B) :- sqrt((X - A)^2 + (Y - B)^2) =< R.

p(X1, Y1, A, X2, Y2, R, X, Y) :-
  XL is X1, XR is X1 + A, YL is Y1, YH is Y1 + A,
  between(XL, XR, X), between(YL, YH, Y),
  in_circle(X2, Y2, R, X, Y).

% 11.06.2012
subseq(M, L) :- append(_, B, L), append(M, _, B).

c(_, []).
c(M, [L|LS]) :- subseq(M, L), c(M, LS).

p(M, [L|LS]) :- subseq(M, L), c(M, [L|LS]), length(M, N),
  not((subseq(P, L), c(P, [L|LS]), length(P, S), S > N)).

% transpose matrix, 05.02.2012
collect_rows([], [], []).
collect_rows([[X|XS] | XSS], [X|R], [XS|S]) :- collect_rows(XSS, R, S).

transpose([[] | _], []).
transpose([R|RS], T) :- collect_rows(R, R1, Rest), transpose(Rest, TR).

% subset
is_subset(A, B) :- not((member(X, A), not(member(X, B)))).

% cartesian product of K sets
cartesian_product([], []).
cartesian_product([XS | XSS], [AS | ASS]) :- member(AS, XS), cartesian_product(XSS, ASS).

% predicate(X, Y), same prime divisors
prime(X) :- X > 1, L is floor(sqrt(X)), not((between(2, L, A), mod(X, A) =:= 0)).

divisors(X, A) :- between(1, X, A), X mod A =:= 0.

is_divisor(X, A) :- X mod A =:= 0.

prime_divisors(X, I) :- divisors(X, I), prime(I).
 
equal_prime_divisors(X, Y) :- not((prime_divisors(X, I), not(is_divisor(Y, I)))).

% is there a cycle in graph passing through vertex v 
gen_path(E, U, W, _, [U, W]) :- member([U, W], E).
gen_path(E, U, W, V, [U | P]) :- member([U, X], E), not(member(X, V)), gen_path(E, X, W, [X | V], P).

cycle(P) :- append(_, [X|XS], P), member(X, XS).

cycle_with_vertex(P, V) :- cycle(P), member(V, P).

gen_vertices([], []).
gen_vertices([[U, W] | E], [U, W | V]) :- gen_vertices(E, V), not(member(U, V)), not(member(W, V)).
gen_vertices([[U, W] | E], [U | V]) :- gen_vertices(E, V), not(member(U, V)), member(W, V).
gen_vertices([[U, W] | E], [W | V]) :- gen_vertices(E, V), not(member(W, V)), member(U, V).
gen_vertices([[U, W] | E], V) :- gen_vertices(E, V), member(W, V), member(U, V).

has_cycle_with_vertex(E, Vertex) :- gen_vertices(E, V),
                                    member(U, V),
                                    member(W, V),
                                    gen_path(E, U, W, [], P),
                                    cycle_with_vertex(P, Vertex).

has_path_with_length(E, L, U, W) :- gen_path(E, U, W, [], P), length(P, LP), LP =< L.

permutation2([], []).
permutation2([A | L1], P) :- permutation2(L1, P1), append(S, F, P1), append(S, [A|F], P).
