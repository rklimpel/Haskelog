module Test where

import Type
import Pretty
import Sub
import Unify

sample1 :: Term
sample1 = Comb "append" [ Var 0, Comb "." [ Var 1, Var 2 ], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]]]
-- Pretty Result: "append(A, [B|C], [1,2])""

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
sample6 = Comb "append" [Var 0, Comb "." [Var 1, Var 4], Comb "." [ Comb "1" [], Comb "." [ Comb "3" [], Comb "[]" [] ]]]