% ex. 1
% regular expression -> ((B ∪ BC)(B ∪ BC))*A.
app2([], L, L).
app2(L, [], L).
app2([A|L1], L2, [A|L3]) :- app2(L1, L2, L3).

can_be_concatenated_with_B(A, B, K) :- 
  app2(B, B, F),
  (
    app2(F, A, K);
    app2(F, S, K),
    can_be_concatenated_with_B(A, B, S)
  ).

can_be_concatenated_with_BC(A, B, C, K) :- 
  app2(B, C, L),
  app2(L, L, F),
  (
    app2(F, A, K);
    app2(F, S, K),
    can_be_concatenated_with_BC(A, B, C, S)
  ).

parse(A, B, C, K) :- 
  can_be_concatenated_with_B(A, B, K);
  can_be_concatenated_with_BC(A, B, C, K).

% ex. 2
% parsing of formulas

% (φ₁ ∨ ~φ₂) ∧ (φ₂ => φ₄) <-> [[φ₁, ∨, [~, φ₂]], ∧, [φ₂, =>, φ₄]]

eval(t, or, f, t) :- !.
eval(f, or, f, f) :- !.
eval(f, or, t, t) :- !.
eval(t, or, t, t) :- !.
eval(t, and, f, f) :- !.
eval(f, and, f, f) :- !.
eval(f, and, t, f) :- !.
eval(t, and, t, t) :- !.
eval(t, imp, f, f) :- !.
eval(f, imp, f, t) :- !.
eval(f, imp, t, t) :- !.
eval(t, imp, t, t) :- !.
eval(t, eq, f, f) :- !.
eval(f, eq, f, t) :- !.
eval(f, eq, t, f) :- !.
eval(t, eq, t, t) :- !.

log_not(t, f).
log_not(f, t).

parse([], _, E) :- !.

parse([[X, O, [not, Y]] | F], [A, B | V], E) :-
  log_not(B, NB),
  eval(A, O, NB, E),
  parse(F, V, E), !.

parse([[X, or, Z] | F], [A, B| V], E) :-
  eval(A, or, B, E),
  parse(F, V, E), !.

parse([[X, and, Z] | F], [A, B| V], E) :-
  eval(A, and, B, E),
  parse(F, V, E), !.

parse([[X, imp, Z] | F], [A, B | V], E) :-
  eval(A, imp, B, E),
  parse(F, V, E), !.

parse([[X, eq, Z] | F], [A, B | V], E) :- 
  eval(A, eq, B, E),
  parse(F, V, E), !.
