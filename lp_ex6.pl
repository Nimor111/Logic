% (a, a) ∈ R iff mem2([a, a], R).
% instead of ∀ a [(a, a) ∈ R] <=> not ∃a : (a, a) not ∈ R
% ( we tell Prolog the exists case)
% mem2(a, A), not(mem2((a, a), R)).
% If this condition is met, then R is not reflexive

% symmetric -> for all a, b from A, R - relation, (ARb, => bRa)
% from universal to existential formula -> there is not a, b from A such that it is not true (aRb => bRa)
% there is a, b from A such that aRb => not(bRa)

mem2(X, [X|_]).
mem2(X, [_|L1]) :- mem2(X, L1).

not_reflexive(A, R) :- mem2(X, A), not(mem2([X, X], R)).
reflexive(A, R) :- not(not_reflexive(A, R)).

not_symmetric(A, R) :- mem2([X, Y], R), not(mem2([Y, X], R)).
symmetric(A, R) :- not(not_symmetric(A, R)).

not_transitive(A, R) :- mem2([X, Y], R), mem2([Y, Z], R), not(mem2([X, Z], R)).
transitive(A, R) :- not(not_transitive(A, R)).

equivalence_relation(A, R) :- reflexive(A, R), symmetric(A, R), transitive(A, R).


% automata
% A = <Σ, Q, s, F, Δ>)
% Δ ⊆ Q X Σ X Q 
% omega from Σ*, ω = [a1, a2, ..., an]

% we start from s and we look at a sequence of edges with letters a1...an
% (s, a1, q1) ∈ Δ, (q1, a2, q2) ∈ Δ, ..., (qn-1, an, qn) ∈ Δ and q1 ∈ F
% we generate all n-tuples (<state, аi, state>)
% we check whether they are correct
% L = [[pi, ai, qi]], i = 1..n

gen_list(Q, [], []).
gen_list(Q, [A|W], [[P1, A, P2] | L]) :- mem2(P1, Q), mem2(P2, Q), gen_list(Q, W, L).

% a path L is not correct if there ∃ element [P1, A, P2] not ∈ Δ or there exist two sequential [P1, A, P2], [P3, B, P4],
% P2 ≠ P3, or the first element is not [S, A1, _] or the last element is [P, An, F1], F1 not ∈ F.

last([X], X).
last([_|L], X) :- last(L, X).

first([X|_], X).

does_not_accept_word([AL, Q, S, F, D], L, W) :- first(W, A), not(append([[P, A, P1]], L1, L)), not(P == S).
does_not_accept_word([AL, Q, S, F, D], [P | L1], [_|W]) :- not(mem2(P, D)).
does_not_accept_word(T, L, W) :- append([_, [A, B], _], W), append([_, [[P1, A, P2], [P3, B, P4]], _], L), not(P2 == P3).
does_not_accept_word([AL, Q, S, F, D], L, W) :- last(W, A), not(append(L1, [[P, A, P1]], L)), not(mem2(P1, F)).

accepts_word(T, L, W) :- not(does_not_accept_word(T, L, W)).
