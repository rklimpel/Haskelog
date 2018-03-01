module Test where

import Type
import Pretty
import Sub
import Unify
import Utils.TermUtils

sample1 :: Term
sample1 = Comb "append" [ Var 0, Comb "." [ Var 1, Var 2 ], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]],Var 3]
-- Pretty Result: "append(A, [B|C], [1,2])""

sample1sub ::Term
sample1sub = Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]]

sample2 :: Term
sample2 = Comb "." [Var 0,Var 1]
-- Pretty Result: "[A,B]"

sample3 :: Subst
sample3 = compose (single 1 (Var 2))(single 0 (Comb "f" [Var 1, Comb "true" []]))
-- Pretty Result: "{A -> f(C, true), B -> C}"

sample4 :: Subst
sample4 = Subst [Replace 0 (Comb "f" [Var 1, Comb "true" []]), Replace 1 (Var 25), Replace 2 (Var 10)]

sample5 :: Term
sample5 = Comb "append" [Var 0, Comb "." [Var 1, Var 2], Comb "." [ Comb "1" [], Comb "." [ Comb "3" [], Comb "[]" [] ]]]

sample6 :: Term
sample6 = Comb "append" [Var 0, Comb "." [Var 1, Var 4], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]],Var 4]

-- SLD Test 1
goalA = [Comb "mutter" [Comb "Olaf" [],Var 0]]
ruleA1 = Comb "mutter" [Comb "Olaf" [],Comb "Mathilda" []] :- []
ruleA2 = Comb "mutter" [Comb "hugo" [],Comb "Mathilda" []] :- []
progA = [rulea1,rulea2]

-- SLD Test 2
goalB = [Comb "append" [Var 0,Var 1,Comb "." [Comb "1" [],Comb "." [Comb "2" [],Comb "[]" []]]]]
ruleB1 = Comb "append" [Comb "[]" [],Var 0,Var 0] :- []
ruleB2 = Comb "append" [Comb "." [Var 0,Var 1],Var 2,Comb "." [Var 0, Var 3]]
progB = [ruleB1,ruleB2]

--SLD Test 3
goalC = [Comb "vater" [Comb "Olaf" []],Var 0]]
ruleC1 = Comb "mutter" [Comb "Olaf" [],Comb "Matilda" []] :- []
ruleC2 = Comb "ehemann" [Comb "Mathilda" [],Comb "Heiner" []] :- []
ruleC3 = Comb "vater" [Var 0,Var 1] :- [Comb "mutter" [Var 0,Var 2],Comb "ehemann" [Var 2,Var 1]]
progC = [ruleC1,ruleC2,ruleC3]