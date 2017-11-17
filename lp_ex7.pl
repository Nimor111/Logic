% 17th november 2017
% List L
% Generate L' with elements from L, where elements can be repeated.
% L = [1, 2, 3]
% 1. How many times is every element of L repeated?
% - generate L-tuples of natural numbers
% - if we have two lists [a₁, a₂, ..., ak], [n₁, n₂, ..., nk]
% [a₁, ... , a₁, (n₁ times), a₂, ..., a₂ (n₂ times), ..., ak...ak (nk times)]
% permute the above list

% map_to_pair(N, A, B) <=> N = 2ٰ^A*(2B + 1).
% we generate a k-tuple for a fixed K > 1
% e.g. for n ∈ N we build [a₁, a₂, ..., ak]
% bijection from N -> N^k
% 1 -> n ∈ N
% 2 -> x ∈ {0, 1, ..., N},  y = N - x

map_to_pair(N, 0, K) :- N mod 2 =:= 1, K is (N - 1)/2.
map_to_pair(N, A, B) :- N mod 2 =:= 0, N1 is N/2, map_to_pair(N1, A1, B), A is A1 + 1.

map_to_tuples(N, 1, [N]).
map_to_tuples(N, K, [A, B | L1]) :- K > 1,
                                    K1 is K - 1,
                                    map_to_tuples(N, K1, [M | L1]),
                                    M1 is M + 1,
                                    map_to_pair(M1, A, B).

% So, if we have a bijection fk-1 : N → N we define fk : N → N^k as
% fk(n) = (a, b, c₂, c₃, ... ck-1), fk-1(n) = (c₁, c₂, ..., ck-1)
% map_to_pair(c₁) = (a, b)

% example → 7 = 2^0(2*3+1), f₂(7) = (0, 3), f₃(7) = (f₂(1), 3) for f₂(1) = 2^0(2*0 + 1) = (0, 0, 3)
% example2 → 48 = 2^4 * 3 = 2^4(2*1 + 1)
% f₂(48) = (4, 1)
% f₃(48) = (f₂(4), 1)
% f₂(5) = (0, 2) (5 = 2⁰ * (2*2 + 1))
% f₄(48) = (f₂(0+1), 2, 1) = (0, 0, 2, 1)

between2(I, J, I) :- I =< J.
between2(I, J, K) :- I + 1 =< J, I1 is I + 1, between2(I1, J, K).

mem2(X, [X]).
mem2(X, [_|L]) :- mem2(X, L).

% gen_tuples(N, K, L) - generates in L all K-tuples with elements {0...N}
gen_tuples(N, 0, []).
gen_tuples(N, K, [A|L1]) :- K > 0,
                            between2(0, N, A), % gen first element
                            K1 is K - 1,
                            N1 is N - A,
                            gen_tuples(N1, K1, L1).

% We have a predicate gen(A), which generates A ( some object ).
% We know that a finite number of objects satisfy gen.
% gen enumerates them all and stops (returns false).
% We want to generate a list of all elements which satisfy gen.
% Accumulator(CurrResult, Result), if there is no elements A : gen(A) is true && A not ∈ CurrRes, then CurrRes contains
% all needed objects
accumulate(CurrResult, CurrResult) :- not(not_ready(CurrResult, A)).
accumulate(CurrResult, Res) :- not_ready(CurrResult, A), accumulate([A | CurrResult], Res).

accumulate_all(Res) :- accumulate([], Res).

not_ready(CurrResult, A) :- gen(A), not(mem2(A, CurrResult)).
