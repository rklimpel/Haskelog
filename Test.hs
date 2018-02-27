module Test where

import Type
import Pretty

sample :: Term
sample = Comb "append" [Var 0,Comb "." [Var 1, Var 2], Comb "." [Comb "2" [], Comb "[]" []]]
