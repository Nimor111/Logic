% Problem 1
% Find the last element of a list.

my_last([X], X).
my_last([_|L], X) :- my_last(L, X).

% Problem 2
% Find the last but one element of a list.

my_but_last([Y, _], Y).
my_but_last([_|L], Y) :- my_but_last(L, Y).

% Problem 3
% Find the K'th element of a list.

element_at(0, [X|_], X).
element_at(K, [_|T], X) :- K1 is K-1, element_at(K1, T, X).

% Problem 4
% Find the number of elements of a list.

len([], 0).
len([_|T], X) :- len(T, X1), X is X1+1.

% Problem 5
% Reverse a list.

reverse([], []).
reverse([H|T], L) :- reverse(T, L1), append(L1, [H], L).

% Problem 6
% Find out whether a list is a palindrome.

palindrome([]).
palindrome(L) :- reverse(L, L).
% palindrome(L) :- reverse(L, L1), L1 == L.
 
% Problem 7
% Flatten a nested list structure.

my_flatten([], []).
my_flatten([H|T], [H|X]) :- not(is_list(H)), my_flatten(T, X).
my_flatten([H|T], X) :- is_list(H), my_flatten(H, X1), my_flatten(T, X2), append(X1, X2, X).
