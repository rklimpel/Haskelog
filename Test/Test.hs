module Test.Test where

import Pretty
import Type
import Sub
import Unify
import SLDTree
import Utils.StringUtils
import Test.Samples


prettyTest :: IO()
prettyTest = do

  -- pretty Test Variable	
  putStr "\n::Variables\n"

  putStr "\npretty (Var 2) \n--> "
  putStrLn(pretty (Var 2))

  putStr "\npretty (Var 24) \n--> "
  putStrLn(pretty (Var 24))

  putStrLn seperator 

  -- pretty Test Term
  putStr "\n::Terms\n"

  putStr "\n(Comb \"append\" [Var 0, Var 1]) \n--> "
  putStrLn (pretty (Comb "append" [Var 0, Var 1]))

  putStr "\n(Comb \"f\" [(Comb \"p\" [Var 0, Var 1]), Var 2]) \n--> "
  putStrLn (pretty (Comb "f" [(Comb "p" [Var 0, Var 1]), Var 2]))

  putStr "\n(Comb \"p\" [(Comb \"p\" [Var 0, Var 1]), (Comb \"q\" [Var 2, Var 3])]) \n--> "
  putStrLn (pretty (Comb "p" [(Comb "p" [Var 0, Var 1]), (Comb "." [Var 2, Var 3])]))

  putStr ("\n" ++ (show term1) ++ "\n--->" )
  putStrLn (pretty term1)

  putStrLn seperator

  -- pretty Test Rule
  {- 
  putStr "\n>> Rule <<\n\nRule 1: ((Comb \"p\" [Var 0, Var 1]) :- [Comb \"q\" [Var 0, Var 2], Comb \"p\" [Var 2, Var 1]]) \n--> "
  putStrLn(pretty ((Comb "p" [Var 0, Var 1]) :- [Comb "q" [Var 0, Var 2], Comb "p" [Var 2, Var 1]]))
  
  putStr "\n(Comb \"p\" [Var 0, Var 0] :- []) --> "
  putStrLn(pretty (Comb "p" [Var 0, Var 0] :- []))
  putStrLn "-----" 
  -}

  -- pretty Test Program
  {-
  putStr "\n>> Program <<\n\n(Prog [Rule 1, (Comb \"p\" [Var 0, Var 0] :- []), (Comb \"q\" [(Comb \"b\" []), (Comb \"y\" [])] :- [])]) \n--> "
  putStrLn(pretty (Prog [((Comb "p" [Var 0, Var 1]) :- [Comb "q" [Var 0, Var 2], Comb "p" [Var 2, Var 1]]), (Comb "p" [Var 0, Var 0] :- []), (Comb "q" [(Comb "b" []), (Comb "y" [])] :- [])]))
  putStrLn "-----" 
  -}

  -- pretty Test Goal
  {-
  putStr "\n>> Goal <<\n\n(Goal (Comb \"p\" [Var 18, (Comb \"b\" [])])) --> "
  putStrLn(pretty (Goal [(Comb "p" [Var 18, (Comb "b" [])])]))
  putStrLn "-----" 
  -}

  -- pretty Test Subst

  putStr "\n::Substitutions\n"
  putStr "\n(Subst 0 [(1, (Comb \"p\" [Var 0, Var 2]))]) \n--> "
  putStrLn(pretty (Subst [(Replace 1 (Comb "p" [Var 0, Var 2]))]))

  putStr "\nSubst 8 [(1, (Comb \"p\" [Var 0, Var 2])),(23, Var 25)]) \n--> "
  putStrLn(pretty (Subst [(Replace 1 (Comb "p" [Var 0, Var 2])), ( Replace 23 (Var 25))]))

  putStrLn seperator 