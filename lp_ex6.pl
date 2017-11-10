% (a, a) is from R iff mem2([a, a], R).
% instead of for all a (a, a) is from R <=> not exists a such that (a, a) is not from R
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
% A = <sigma, Q, s, F, delta>)
% delta subset of Q X sigma X Q 
% omega from sigma*, omega = [a1, a2, ..., an]
% "ако минаваме така както трябва в А и прочетем цялата дума, стигаме във финално състояние."

% започваме от s и разглеждаме последовтелност от преходи с букви a1,...,an
% (s, a1, q1) ∈ Δ, (q1, a2, q2) ∈ Δ, ..., (qn-1, an, qn) ∈ Δ and q1 ∈ F
% генерираме всички n-орки, (<състояние, аi, <състояние>)
% проверяваме дали са правилни
% L = [[pi, ai, qi]], i = 1..n

gen_list(Q, [], []).
gen_list(Q, [A|W], [[P1, A, P2] | L]) :- mem2(P1, Q), mem2(P2, Q), gen_list(Q, W, L).

% пътят L не е правилен, ако ∃ element [P1, A, P2] not ∈ Δ или има два последователни [P1, A, P2], [P3, B, P4],
% P2 ≠ P3, или пръвият елемент не е [S, A1, _] или последният елемент e [P, An, F1], F1 ∈ F.
