% longest subsequence of elements from L
% L - list of lists 

subset([], []).
subset([A|X], [A|S]) :- subset(X, S).
subset([_|X], S) :- subset(X, S).

is_subsequence([], _).
is_subsequence([X|XS], [X|L]) :- is_subsequence(XS, L), !.
is_subsequence([X|XS], [_|L]) :- is_subsequence([X|XS], L), !.

not_common_subsequence(XS, LS) :- member(L, LS),
                                  not(is_subsequence(XS, L)).

common_subsequence(XS, LS) :- not(not_common_subsequence(XS, LS)).

not_longest_common_subsequence(XS, L, LS) :- subset(L, S),
                                             common_subsequence(S, LS),
                                             length(S, N),
                                             length(XS, N1),
                                             N > N1.

head([X|_], X).

longest_subsequence(L, M) :- head(L, X),
                             subset(X, M),
                             common_subsequence(M, L),
                             not(not_longest_common_subsequence(M, X, L)), !.

% ex. 2 - Relation time

non_symmetric_relation(L) :- member([X, Y], L), not(member([X, Y], L)).
symmetric_relation(L) :- not(non_symmetric_relation(L)).

non_transitive_relation(L) :- member([X, Y], L), member([Y, Z], L), not(member([X, Z], L)).
transitive_relation(L) :- not(non_transitive_relation(L)).

not_in_composition(R1, R2, [X, Z]) :- member([X, Y], R1), not(member([Y, Z], R2)).

is_in_composition(R1, R2, [X, Z]) :- not(not_in_composition(R1, R2, [X, Z])).

gen_composition_element(R1, R2, [X, Z]) :- member([X, Y], R1), member([Y, Z], R2).

accumulate(R1, R2, Curr, Curr) :- not(not_ready(R1, R2, Curr, _)).
accumulate(R1, R2, Curr, Res) :- not_ready(R1, R2, Curr, A), accumulate(R1, R2, [A | Curr], Res).

not_ready(R1, R2, Curr, A) :- gen_composition_element(R1, R2, A), not(member(A, Curr)).

composition(R1, R2, R3) :- accumulate(R1, R2, [], R3), !.
