scale(_, [], []).
scale(C, [H|T], [H1|R]) :- H1 is C*H, scale(C, T, R).

sum_poly([], G, G).
sum_poly(F, [], F).
sum_poly([F1|F], [G1|G], [H|R]) :- H is F1+G1, sum_poly(F, G, R).

split([], [], []).
split([A], [A], []).
split([A, B|L], [A|L1], [B|L2]) :- split(L, L1, L2).

calc_resultodd(Hsum, Heven, Hodd, R) :-
	scale(-1, Hodd, RO),
	scale(-1, Heven, RE),
	sum_poly(RO, RE, R1),
	sum_poly(Hsum, R1, R).

shift_sum(HE, GE, RE) :- sum_poly(HE, [0|GE], RE).

merge([], [], []).
merge([], [H|T], [0, H|R]) :- merge([], T, R).
merge([H|T], [], [H, 0|R]) :- merge(T, [], R).
merge([H1|T1], [H2|T2], [H1, H2|R]) :- merge(T1, T2, R).

product_pol([C], G, H) :- scale(C, G, H).
product_pol(F, [C], H) :- scale(C, F, H).
product_pol(F, G, H) :-
	split(F, FE, FO),
	split(G, GE, GO),
	sum_poly(GE, GO, GS),
	sum_poly(FE, FO, FS),
	product_pol(FE, GE, HE),
	product_pol(FO, GO, HO),
	product_pol(FS, GS, HS),
	calc_resultodd(HS, HE, HO, RO),
	shift_sum(HE, HO, RE),
	merge(RE, RO, H).
