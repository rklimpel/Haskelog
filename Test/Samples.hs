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
var2 = (Var 24)

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
--append([1,2])

-- was rauskommen soll: append(A|[2,3],[3,4,5],[1,2,3,4,5])
term9 :: Term
term9 = Comb "append" [Comb "." [Var 0 , Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]],
                       Comb "." [Comb "3" [], Comb "." [Comb "4" [], Comb "." [Comb "5" [], Comb "[]" []]]],
                       Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "." [Comb "4" [], Comb "." [Comb "5" [], Comb "[]" []]]]]]]
--Das Kommt auch raus nice

-- was rauskommen sollte: append([A,2,3,B,4], [], [[1],2,3,[4],4])
term10 :: Term
term10 = Comb "append"[Comb "." [Var 0 , Comb "." [Comb "2" [],Comb "." [Comb "3" [],Comb "." [Var 1, Comb "." [Comb "4" [], Comb "[]" [] ] ] ] ] ] ]
                      -- [],
                      -- [Comb "." [Comb "." [Comb "1" [], Comb "[]" []] , Comb "2" [], Comb "3" [], Comb "." [Comb "4" [], Comb "[]" []], Comb "4" [] ], Comb "[]" []]]
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


-- SUBSTITUTIONS

-- "{A -> f(C, true), B -> C}"
sub1 :: Subst
sub1 = compose (single 1 (Var 2))(single 0 (Comb "f" [Var 1, Comb "true" []]))

-- "{A -> f(B,true),B -> Z,C -> K}"
sub2 :: Subst
sub2 = Subst [Replace 0 (Comb "f" [Var 1, Comb "true" []]), Replace 1 (Var 25), Replace 2 (Var 10)]

sub3 :: Subst
sub3 = Subst [(Replace 1 (Comb "p" [Var 0, Var 2]))]

sub4 :: Subst
sub4 = Subst [(Replace 1 (Comb "p" [Var 0, Var 2])), ( Replace 23 (Var 25))]


-- EXERCISE 4

-- SLD Test 1
goalA = Goal [Comb "mutter" [Comb "Olaf" [],Var 0]]
ruleA1 = Comb "mutter" [Comb "Olaf" [],Comb "Mathilda" []] :- []
ruleA2 = Comb "mutter" [Comb "hugo" [],Comb "Mathilda" []] :- []
progA = Prog [ruleA1,ruleA2]

-- SLD Test 2
goalB = Goal [Comb "append" [Var 0,Var 1,Comb "." [Comb "1" [],Comb "." [Comb "2" [],Comb "[]" []]]]]
ruleB1 = Comb "append" [Comb "[]" [],Var 0,Var 0] :- []
ruleB2 = (Comb "append" [Comb "." [Var 0,Var 1],Var 2,Comb "." [Var 0, Var 3]]) :- []
progB = Prog [ruleB1,ruleB2]

--SLD Test 3
goalC = Goal [Comb "vater" [Comb "Olaf" [],Var 0]]
ruleC1 = Comb "mutter" [Comb "Olaf" [],Comb "Lara" []] :- []
ruleC2 = Comb "ehemann" [Comb "Lara" [],Comb "Heiner" []] :- []
ruleC3 = Comb "vater" [Var 0,Var 1] :- [Comb "mutter" [Var 0,Var 2],Comb "ehemann" [Var 2,Var 1]]
progC = Prog [ruleC1,ruleC2,ruleC3]

sld1 :: SLDTree
sld1 = sld progA goalA

sld2 :: SLDTree
sld2 = sld progB goalB

sld3 :: SLDTree
sld3 = sld progC goalC
