app2([], L, L).
app2(L, [], L).
app2([A|L1], L2, [A|L3]) :- app2(L1, L2, L3).

cartesian_product1(_, [], []).
cartesian_product1(A,[X|T],[[A,X]|T1]):-cartesian_product1(A,T,T1).

cartesian_product([], _, []).
cartesian_product([A | T], B, L3) :- cartesian_product1(A, B, L1), cartesian_product(T, B, L2), app2(L1, L2, L3).

cartesian_square(A, A2) :- cartesian_product(A, A, A2).

gen_subsets([], []).
gen_subsets([A | L1], [A | L2]) :- gen_subsets(L1, L2).
gen_subsets([_ | L1], L2) :- gen_subsets(L1, L2).
