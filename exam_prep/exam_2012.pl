% 05.02.2012


% task 1
subset([], []).
subset([A|L], [A|S]) :- subset(L, S).
subset([_|L], S) :- subset(L, S).

mem2(X, [X|_]).
mem2(X, [_|L]) :- mem2(X, L).

app2([], L, L).
app2(L, [], L).
app2([A|L1], L2, [A|L3]) :- app2(L1, L2, L3).

% multisets?
is_subset([], _).
is_subset([A|XS], YS) :- mem2(A, YS), is_subset(XS, YS).

has_special_element(X, M, L) :- mem2(Y, M),
                                Sub is X-Y,
                                Mul is X*Y,
                                Sum is X+Y,
                                is_subset([Sub, Mul, Sum], L).

no_special_element(X, M, L) :- not(has_special_element(X, M, L)).

every_special_element(M, L) :- app2(_, [X|_], M), not(no_special_element(X, M, L)).

subsets_with_special_elements(L, M) :- subset(L, M), every_special_element(M, L).

% task 2 
% get one col
col([], [], []).
col([[X|XS]|M], [X|C], [XS|M1]) :- col(M, C, M1).

% transpose matrix
t([[]|_], []).
t(M, [X|T]) :- col(M, X, M1), t(M1, T).
