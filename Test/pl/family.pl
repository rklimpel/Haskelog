vater(laura,justin).
vater(justin,kobe).

mutter(olaf,lara).
mutter(lara,chanti).
mutter(kobe,anja).
mutter(chanti,anja).
mutter(franzjosef,carla).
mutter(dieter,carla).
mutter(eins,anette).
mutter(zwei,anette).
mutter(drei,anette).

ehemann(lara,heiner).
ehemann(chanti,rainer).
ehemann(anja,franzjosef).
ehemann(carla,gustaf).
ehemann(anette,dieter).

vater(S,V) :- mutter(S,M),ehemann(M,V).

eltern(K,M) :- mutter(K,M).
eltern(K,V) :- vater(K,V).

oma(E,O) :- eltern(E,P),mutter(P,O).
opa(E,O) :- eltern(E,P),vater(P,O).

uropa(UE,UO) :- eltern(UE,E),eltern(E,G),vater(G,UO).
uroma(UE,UO) :- eltern(UE,E),eltern(E,G),mutter(G,UO).

vorfahre(N,V) :- eltern(N,V).
vorfahre(N,V) :- eltern(N,E),vorfahre(E,V).