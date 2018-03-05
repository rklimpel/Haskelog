kandidat(meier).
kandidat(mueller).
kandidat(schroeder).
kandidat(schulz).

verschieden(meier,mueller).
verschieden(meier,schroeder).
verschieden(meier,schulz).
verschieden(mueller,schroeder).
verschieden(mueller,schulz).
verschieden(mueller,meier).
verschieden(schroeder,schulz).
verschieden(schroeder,meier).
verschieden(schroeder,mueller).
verschieden(schulz,meier).
verschieden(schulz,mueller).
verschieden(schulz,schroeder).

keinedoppelten(V,S,K) :-
    verschieden(V,S),
    verschieden(V,K),
    verschieden(S,K).

nichtMeier(mueller).
nichtMeier(schroeder).
nichtMeier(schulz).
nichtMueller(meier).
nichtMueller(schroeder).
nichtMueller(schulz).
nichtSchroeder(mueller).
nichtSchroeder(meier).
nichtSchroeder(schulz).
nichtSchulz(mueller).
nichtSchulz(meier).
nichtSchulz(schroeder).

muellerimvorstand(V,S,K) :- V = mueller,kandidat(S),kandidat(K);
                            S = mueller,kandidat(V),kandidat(K);
                            K = mueller,kandidat(V),kandidat(S).

meierimvorstand(V,S,K) :- V = meier,kandidat(S),kandidat(K);
                          S = meier,kandidat(V),kandidat(K);
                          K = meier,kandidat(V),kandidat(S).

meiernichtimvorstand(V,S,K) :- nichtMeier(V),nichtMeier(S),nichtMeier(K).

problemEins(V,S,K) :- muellerimvorstand(V,S,K),nichtMeier(V),nichtMeier(S),nichtMeier(K).
problemEins(V,S,K) :- meierimvorstand(V,S,K),nichtMueller(V),nichtMueller(S),nichtMueller(K).

problemZwei(V,S,K) :- V = schulz, kandidat(S), kandidat(K).
problemZwei(V,S,K) :- nichtSchulz(V),nichtMueller(V),nichtMueller(S),nichtMueller(K).

problemDrei(V,S,K) :- meierimvorstand(V,S,K),kandidat(V),kandidat(S),kandidat(K).
problemDrei(V,S,K) :- meiernichtimvorstand(V,S,K),nichtSchroeder(V),nichtSchroeder(S),nichtSchroeder(K).

problemVier(V,S,K) :- S = schulz,nichtMeier(V),nichtMeier(K).
problemVier(V,S,K) :- nichtSchulz(S),kandidat(V),kandidat(K).

problemFunf(V,S,K) :- nichtSchroeder(V),kandidat(S),kandidat(K).
problemFunf(V,S,K) :- V = schroeder,nichtSchulz(S),nichtSchulz(K).

vorsitzender(V) :- kandidat(V).
schriftfuehrer(S) :- kandidat(S).
kassenwart(K) :- kandidat(K).

vorstand(V,S,K) :- vorsitzender(V),schriftfuehrer(S),kassenwart(K),
                   keinedoppelten(V,S,K),
                   problemEins(V,S,K),problemZwei(V,S,K),problemDrei(V,S,K),
                   problemVier(V,S,K),problemFunf(V,S,K).
