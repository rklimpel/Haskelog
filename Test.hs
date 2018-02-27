module Test where

import Type
import Pretty

sample :: Term
sample = Comb "append" [ Var 0, Comb "." [ Var 1, Var 2 ], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" [] ]]]
-- Var 0, [Var 1, Var 2] , [1,[2,()]]
-- "append(A, [B|C], [1,2])""


sample2 :: Term
sample2 = Comb "." [Var 0,Var 1]
-- "[A,B]"

-- Var 0,[Var 1,Var 2],[1,[2,[]]]
