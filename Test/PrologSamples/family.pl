vater(laura,justin).
vater(justin,kobe).

mutter(olaf,lara).
mutter(lara,chanti).
mutter(kobe,anja).

ehemann(lara,heiner).
ehemann(chanti,rainer).
ehemann(anja,franz-josef).

vater(S,V) :- mutter(S,M),ehemann(M,V).
eltern(K,M) :- mutter(K,M).
eltern(K,V) :- vater(K,V).
oma(E,O) :- eltern(E,P),mutter(P,O).
opa(E,O) :- eltern(E,P),vater(P,O).
uropa(UE,UO) :- eltern(UE,E),eltern(P,G),vater(G,UO).
uroma(UE,UO) :- eltern(UE,E),eltern(P,G),mutter(G,UO).