module Test.Samples where

import Type
import Pretty
import Utils.TermUtils
import Sub
import SLDTree


-- VARIABLES

var1 :: Term
var1 = (Var 1)

var2 :: Term
var2 = (Var 25)

var3 :: Term
var3 = (Var 26)

var4 :: Term
var4 = (Var 278)

var5 :: Term
var5 = (Var 1526439)

-- TERMS

-- "append(A, [B|C], [1,2])"
term1 :: Term
term1 = Comb "append" [ Var 0, Comb "." [ Var 1, Var 2 ], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]],Var 3]

-- "[1,2]"
term2 ::Term
term2 = Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]]

-- "[A,B]"
term3 :: Term
term3 = Comb "." [Var 0,Var 1]

-- "append(A,[B|C],[1,3])"
term4 :: Term
term4 = Comb "append" [Var 0, Comb "." [Var 1, Var 2], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]],Var 17]

-- "append(A,[B|E],[1,3])"
term5 :: Term
term5 = Comb "append" [Var 0, Comb "." [Var 1, Var 4], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]],Var 4]

term6 :: Term
term6 = (Comb "f" [(Comb "p" [Var 0, Var 1]), Var 2])

term7 :: Term
term7 = (Comb "p" [(Comb "p" [Var 0, Var 1]), (Comb "." [Var 2, Var 3])])

term8 :: Term
term8 = Comb "append"[Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "." [Comb "3" [], Comb "[]" [] ]]]]

-- append(A|[2,3],[3,4,5],[1,2,3,4,5])
term9 :: Term
term9 = Comb "append" [Comb "." [Var 0 , Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]],
                       Comb "." [Comb "3" [], Comb "." [Comb "4" [], Comb "." [Comb "5" [], Comb "[]" []]]],
                       Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "." [Comb "4" [], Comb "." [Comb "5" [], Comb "[]" []]]]]]]

-- append([A,2,3,B,4], [], [[1],2,3,[4],4])
term10 :: Term
term10 = Comb "append" [Comb "." [Var 0 , Comb "." [Comb "2" [],Comb "." [Comb "3" [],Comb "." [Var 1, Comb "." [Comb "4" [], Comb "[]" [] ] ] ] ] ] ]
             --   Comb "." [Comb "1" [], Comb "[]" []], Comb "2" [], Comb "3" [], Comb "." [Comb "4" [], Comb "[]" []], Comb "4" [] ], Comb "[]" []]]

term105 :: Term
term105 = Comb "append" [Comb "." [Comb "1" [], Comb "[]" []] , Comb "2" [], Comb "3" [], Comb "." [Comb "4" [], Comb "[]" []], Comb "4" []]

term11 :: Term
term11 =  Comb "append"[Comb "." [Var 0 , Comb "." [Comb "2" [],Comb "." [Comb "3" [],Comb "." [Var 1, Comb "." [Comb "4" [], Comb "[]" [] ] ] ] ] ],
                        Comb "[]" [],
                        Comb "." [Comb "." [Comb "1" [], Comb "[]" []] , Comb "2" [], Comb "3" [], Comb "." [Comb "4" [], Comb "[]" []], Comb "4" [] , Comb "[]" []] ]

-- "vater(Olaf,A)"
term12 :: Term
term12 = (Comb "vater" [Comb "Olaf" [],Var 0])

-- "vater(A,B)"
term13 :: Term
term13 = (Comb "vater" [Var 5,Var 6])

--Sonder flall append([A,B,c],[],[1,2,3])
term14 :: Term
term14 = Comb "append" [Comb "." [Var 0, Comb "." [Var 1, Var 2]],
                      Comb "[]" [],
                      Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]] ]]


-- SUBSTITUTIONS

-- "{A -> f(C, true), B -> C}"
sub1 :: Subst
sub1 = compose (single 1 (Var 2))(single 0 (Comb "f" [Var 1, Comb "true" []]))

-- "{A -> f(B,true),B -> Z,C -> K}"
sub2 :: Subst
sub2 = Subst [Replace 0 (Comb "f" [Var 1, Comb "true" []]), Replace 1 (Var 25), Replace 2 (Var 10)]

--"{B ->p(A,C)}"
sub3 :: Subst
sub3 = Subst [(Replace 1 (Comb "p" [Var 0, Var 2]))]

--"{B ->p(A,C),X-> Z}"
sub4 :: Subst
sub4 = Subst [(Replace 1 (Comb "p" [Var 0, Var 2])), ( Replace 23 (Var 25))]

--"{B->p([1,2]), [1,3] -> Z}"
sub5 :: Subst
sub5 = Subst[(Replace 1 (Comb "p" [Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "[]" [] ] ]]) ) ]


-- EXERCISE 4

-- SLD Test 1
goalA = Goal [Comb "mutter" [Comb "Olaf" [],Var 0]]
ruleA1 = Comb "mutter" [Comb "Olaf" [],Comb "Mathilda" []] :- []
ruleA2 = Comb "mutter" [Comb "hugo" [],Comb "Mathilda" []] :- []
progA = Prog [ruleA1,ruleA2]

-- SLD Test 2
goalB = Goal [Comb "append" [Var 0,Var 1,Comb "." [Comb "1" [],Comb "." [Comb "2" [],Comb "[]" []]]]]
goalB2 = Goal [Comb "member" [Var 0,Comb "." [Comb "1" [],Comb "." [Comb "2" [],Comb "[]" []]]]]
ruleB1 = Comb "append" [Comb "[]" [],Var 0,Var 0] :- []
ruleB2 = (Comb "append" [Comb "." [Var 0,Var 1],Var 2,Comb "." [Var 0, Var 3]]) :- []
ruleB3 = (Comb "delete" [Var 0,Var 1,Var 2]) :- [Comb "append" [Var 3,Comb "." [Var 0,Var 4],Var 1],Comb "append" [Var 3,Var 4,Var 2]] 
ruleB4 = (Comb "member" [Var 0,Comb "." [Var 0,Var 10]]) :- []
ruleB5 = (Comb "member" [Var 0,Comb "." [Var 10,Var 1]]) :- [Comb "member" [Var 0,Var 1]]
ruleB6 = (Comb "member2" [Var 0,Var 1]) :- [Comb "delete" [Var 0,Var 1,Var 2],Comb "member" [Var 0,Var 2]]
progB = Prog [ruleB1,ruleB2,ruleB3,ruleB4,ruleB5,ruleB6]



--SLD Test 3
goalC = Goal [Comb "vater" [Comb "olaf" [],Var 0]]
ruleC1 = Comb "mutter" [Comb "olaf" [],Comb "lara" []] :- []
ruleC2 = Comb "ehemann" [Comb "lara" [],Comb "heiner" []] :- []
ruleC3 = Comb "vater" [Var 5,Var 1] :- [Comb "mutter" [Var 5,Var 2],Comb "ehemann" [Var 2,Var 1]]
progC = Prog [ruleC1,ruleC2,ruleC3]

--SLD Test XTream
goalD = Goal [Comb "oma" [Comb "Olaf" [],Var 0]]
ruleD1 = Comb "mutter" [Comb "Olaf" [],Comb "Lara" []] :- []
ruleD2 = Comb "ehemann" [Comb "Lara" [],Comb "Heiner" []] :- []
ruleD3 = Comb "mutter" [Comb "Lara" [], Comb "Chanti" []] :- []
ruleD4 = Comb "vater" [Var 0,Var 1] :- [Comb "mutter" [Var 0,Var 2], Comb "ehemann" [Var 2,Var 1]]
ruleD5 = Comb "oma" [Var 0, Var 1] :- [Comb "mutter" [Var 0,Var 2], Comb "mutter" [Var 2, Var 1]]
ruleD6 = Comb "oma" [Var 0, Var 1] :- [Comb "vater" [Var 0,Var 2], Comb "mutter" [Var 2, Var 1]]
progD = Prog [ruleD1,ruleD2,ruleD3,ruleD4,ruleD5,ruleD6]

--Der SDL rechent oma der SDL sehr schnell
goalE   = Goal [Comb "uropa" [Var 0,Var 1]]
ruleE1  = Comb "vater" [Comb "Laura" [], Comb "Justin" []] :- []
ruleE2  = Comb "vater" [Comb "Justin" [], Comb "Kobe" []] :- []
ruleE3  = Comb "mutter" [Comb "Olaf" [],Comb "Lara" []] :- []
ruleE4  = Comb "ehemann" [Comb "Lara" [],Comb "Heiner" []] :- []
ruleE5  = Comb "mutter" [Comb "Lara" [], Comb "Chanti" []] :- []
ruleE12 = Comb "ehemann" [Comb "Chanti" [], Comb "Rainer" []] :- []
ruleE13 = Comb "mutter" [Comb "Chanti" [],Comb "Anja" []] :- []
ruleE14 = Comb "mutter" [Comb "Kobe" [],Comb "Anja" []] :- []
ruleE15 = Comb "ehemann" [Comb "Anja" [],Comb "Franz-Josef" []] :- []
ruleE6  = Comb "vater" [Var 0, Var 1] :- [Comb "mutter" [Var 0,Var 2], Comb "ehemann" [Var 2,Var 1]]
ruleE7  = Comb "eltern" [Var 0, Var 1] :- [Comb "mutter" [Var 0, Var 1]]
ruleE8  = Comb "eltern" [Var 0, Var 1] :- [Comb "vater" [Var 0, Var 1]]
ruleE9  = Comb "oma" [Var 0, Var 1] :- [Comb "eltern" [Var 0, Var 2],Comb "mutter" [Var 2, Var 1]]
ruleE10 = Comb "opa" [Var 0, Var 1] :- [Comb "eltern" [Var 0, Var 2],Comb "vater" [Var 2, Var 1]]
ruleE11 = Comb "uropa" [Var 0, Var 1] :- [Comb "eltern" [Var 0, Var 2],Comb "eltern" [Var 2, Var 3], Comb "vater" [Var 3,Var 1]]
progE   = Prog [ruleE1,ruleE2,ruleE3,ruleE4,ruleE4,ruleE5,ruleE6,ruleE7,ruleE8,ruleE9,ruleE10,
                ruleE11,ruleE12,ruleE13,ruleE14,ruleE15]


--THis Sld tree with 2 fathers
--Chantal donest remeber anything of the specail night
goalF   = Goal [Comb "vater" [Comb "Lara" [],Var 0],Comb "vater" [Comb "Olaf" [],Var 1]]
ruleF1  = Comb "mutter" [Comb "Olaf" [],Comb "Lara" []] :- []
ruleF2  = Comb "ehemann" [Comb "Lara" [],Comb "Heiner" []] :- []
ruleF3  = Comb "ehemann" [Comb "Chanti" [], Comb "ITS JOHN CENA"  []]:- []
ruleF4  = Comb "mutter" [Comb "Lara" [], Comb "Chanti" []] :- []
ruleF5  = Comb "vater" [Comb "Lara" [], Comb "Kevin" []]  :- []
ruleF6  = Comb "vater" [Var 0,Var 1] :- [Comb "mutter" [Var 0,Var 2], Comb "ehemann" [Var 2,Var 1]]
ruleF7  = Comb "oma" [Var 0, Var 1] :- [Comb "mutter" [Var 0,Var 2], Comb "mutter" [Var 2, Var 1]]
ruleF8  = Comb "oma" [Var 0, Var 1] :- [Comb "vater" [Var 0,Var 2], Comb "mutter" [Var 2, Var 1]]
progF   = Prog [ruleF1,ruleF2,ruleF3,ruleF4,ruleF5,ruleF6, ruleF7,ruleF8]


sld1 :: SLDTree
sld1 = sld progA goalA

sld2 :: SLDTree
sld2 = sld progB goalB

sld3 :: SLDTree
sld3 = sld progC goalC

sld4 :: SLDTree
sld4 = sld progB goalB2


goalVorstand :: Goal
goalVorstand = Goal [Comb "problemEins" [Var 0,Var 1,Var 2]]

progVorstand :: Prog
progVorstand = Prog [Comb "kandidat" [Comb "meier" []] :- [],Comb "kandidat" [Comb "mueller" []] :- [],Comb "kandidat" [Comb "schroeder" []] :- [],Comb "kandidat" [Comb "schulz" []] :- [],Comb "verschieden" [Comb "meier" [],Comb "mueller" []] :- [],Comb "verschieden" [Comb "meier" [],Comb "schroeder" []] :- [],Comb "verschieden" [Comb "meier" [],Comb "schulz" []] :- [],Comb "verschieden" [Comb "mueller" [],Comb "schroeder" []] :- [],Comb "verschieden" [Comb "mueller" [],Comb "schulz" []] :- [],Comb "verschieden" [Comb "mueller" [],Comb "meier" []] :- [],Comb "verschieden" [Comb "schroeder" [],Comb "schulz" []] :- [],Comb "verschieden" [Comb "schroeder" [],Comb "meier" []] :- [],Comb "verschieden" [Comb "schroeder" [],Comb "mueller" []] :- [],Comb "verschieden" [Comb "schulz" [],Comb "meier" []] :- [],Comb "verschieden" [Comb "schulz" [],Comb "mueller" []] :- [],Comb "verschieden" [Comb "schulz" [],Comb "schroeder" []] :- [],Comb "keinedoppelten" [Var 0,Var 1,Var 2] :- [Comb "verschieden" [Var 0,Var 1],Comb "verschieden" [Var 0,Var 2],Comb "verschieden" [Var 1,Var 2]],Comb "nichtMeier" [Comb "mueller" []] :- [],Comb "nichtMeier" [Comb "schroeder" []] :- [],Comb "nichtMeier" [Comb "schulz" []] :- [],Comb "nichtMueller" [Comb "meier" []] :- [],Comb "nichtMueller" [Comb "schroeder" []] :- [],Comb "nichtMueller" [Comb "schulz" []] :- [],Comb "nichtSchroeder" [Comb "mueller" []] :- [],Comb "nichtSchroeder" [Comb "meier" []] :- [],Comb "nichtSchroeder" [Comb "schulz" []] :- [],Comb "nichtSchulz" [Comb "mueller" []] :- [],Comb "nichtSchulz" [Comb "meier" []] :- [],Comb "nichtSchulz" [Comb "schroeder" []] :- [],Comb "muellerimvorstand" [Var 0,Var 1,Var 2] :- [Comb "muellerimvorstand" [Comb "mueller" [],Comb "kandidat" [Var 1],Comb "kandidat" [Var 2]]],Comb "muellerimvorstand" [Var 0,Var 1,Var 2] :- [Comb "muellerimvorstand" [Comb "mueller" [],Comb "kandidat" [Var 0],Comb "kandidat" [Var 2]]],Comb "muellerimvorstand" [Var 0,Var 1,Var 2] :- [Comb "muellerimvorstand" [Comb "mueller" [],Comb "kandidat" [Var 0],Comb "kandidat" [Var 1]]],Comb "meierimvorstand" [Var 0,Var 1,Var 2] :- [Comb "meierimvorstand" [Comb "meier" [],Comb "kandidat" [Var 1],Comb "kandidat" [Var 2]]],Comb "meierimvorstand" [Var 0,Var 1,Var 2] :- [Comb "meierimvorstand" [Comb "meier" [],Comb "kandidat" [Var 0],Comb "kandidat" [Var 2]]],Comb "meierimvorstand" [Var 0,Var 1,Var 2] :- [Comb "meierimvorstand" [Comb "meier" [],Comb "kandidat" [Var 0],Comb "kandidat" [Var 1]]],Comb "meiernichtimvorstand" [Var 0,Var 1,Var 2] :- [Comb "nichtMeier" [Var 0],Comb "nichtMeier" [Var 1],Comb "nichtMeier" [Var 2]],Comb "problemEins" [Var 0,Var 1,Var 2] :- [Comb "muellerimvorstand" [Var 0,Var 1,Var 2],Comb "nichtMeier" [Var 0],Comb "nichtMeier" [Var 1],Comb "nichtMeier" [Var 2]],Comb "problemEins" [Var 0,Var 1,Var 2] :- [Comb "meierimvorstand" [Var 0,Var 1,Var 2],Comb "nichtMueller" [Var 0],Comb "nichtMueller" [Var 1],Comb "nichtMueller" [Var 2]],Comb "problemZwei" [Var 0,Var 1,Var 2] :- [Comb "problemZwei" [Comb "schulz" [],Comb "kandidat" [Var 1],Comb "kandidat" [Var 2]]],Comb "problemZwei" [Var 0,Var 1,Var 2] :- [Comb "nichtSchulz" [Var 0],Comb "nichtMueller" [Var 0],Comb "nichtMueller" [Var 1],Comb "nichtMueller" [Var 2]],Comb "problemDrei" [Var 0,Var 1,Var 2] :- [Comb "meierimvorstand" [Var 0,Var 1,Var 2],Comb "kandidat" [Var 0],Comb "kandidat" [Var 1],Comb "kandidat" [Var 2]],Comb "problemDrei" [Var 0,Var 1,Var 2] :- [Comb "meiernichtimvorstand" [Var 0,Var 1,Var 2],Comb "nichtSchroeder" [Var 0],Comb "nichtSchroeder" [Var 1],Comb "nichtSchroeder" [Var 2]],Comb "problemVier" [Var 0,Var 1,Var 2] :- [Comb "problemVier" [Var 0,Comb "schulz" [],Var 2],Comb "nichtMeier" [Var 0],Comb "nichtMeier" [Var 2]],Comb "problemVier" [Var 0,Var 1,Var 2] :- [Comb "nichtSchulz" [Var 1],Comb "kandidat" [Var 0],Comb "kandidat" [Var 2]],Comb "problemFunf" [Var 0,Var 1,Var 2] :- [Comb "nichtSchroeder" [Var 0],Comb "kandidat" [Var 1],Comb "kandidat" [Var 2]],Comb "problemFunf" [Var 0,Var 1,Var 2] :- [Comb "problemFunf" [Comb "schroeder" [],Var 1,Var 2],Comb "nichtSchulz" [Var 1],Comb "nichtSchulz" [Var 2]],Comb "vorsitzender" [Var 0] :- [Comb "kandidat" [Var 0]],Comb "schriftfuehrer" [Var 1] :- [Comb "kandidat" [Var 1]],Comb "kassenwart" [Var 2] :- [Comb "kandidat" [Var 2]],Comb "vorstand" [Var 0,Var 1,Var 2] :- [Comb "vorsitzender" [Var 0],Comb "schriftfuehrer" [Var 1],Comb "kassenwart" [Var 2],Comb "keinedoppelten" [Var 0,Var 1,Var 2],Comb "problemEins" [Var 0,Var 1,Var 2],Comb "problemZwei" [Var 0,Var 1,Var 2],Comb "problemDrei" [Var 0,Var 1,Var 2],Comb "problemVier" [Var 0,Var 1,Var 2],Comb "problemFunf" [Var 0,Var 1,Var 2]]]