append([],L,L).
append([E|R],L,[E|RL]) :- append(R,L,RL).

last(L,E) :- append(_,[E],L).

member(E,L) :- append(_,[E|_],L).

delete(E,L,R) :- append(L1,[E|L2],L), append(L1,L2,R).

sublist(T,L) :- append(_,L2,L), append(T,_,L2).

lookup(K,KVs,V) :- append([K],[V],X),sublist(X,KVs).

member2(E,L) :- delete(E,L,N),member(E,N).

reverse(Xs,Ys) :- reverseH(Xs,Ys).

reverseH([],[]).
reverseH([H|T],Ys) :- reverse(T,Rev),append(Rev,[H],Ys).
