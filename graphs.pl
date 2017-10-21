% graphs

add_edge([], (V, W), [(V, W)]).
add_edge(G, (V, W), [(V, W) | G]).

is_vertex(V, [(W, X) | G]) :- V == W ; V == X; is_vertex(V, G).

successors(_, [], []).
successors(V, [(A, B) | G], [B | R]) :- is_vertex(V, [(A, B)|G]), V == A, successors(V, G, R).
successors(V, [(_, _) | G], R) :- successors(V, G, R).
