module Test.TestViews where

import Pretty
import Type
import Sub
import Unify
import SLDTree

-- Test Show Helper:

showPrettyTest :: Show a => Pretty a => a -> IO()
showPrettyTest a = do
  putStr ("\npretty (" ++ (show a) ++ ") \n  |  \n  V\n")
  putStrLn (pretty a)
  putStrLn ""

showApplySubstTest :: Subst -> Term -> IO()
showApplySubstTest s t = do
  putStr ("\napply " ++ (pretty s) ++ " on " ++ (pretty t) ++ "\n  |  \n  V\n")
  putStrLn (pretty (apply s t))  
  putStrLn ""

showComposeSubstTest :: Subst -> Subst -> IO()
showComposeSubstTest s1 s2 = do
  putStr ("\n" ++ (pretty s1) ++ " o " ++ (pretty s2) ++ "\n  |  \n  V\n")
  putStrLn (pretty (compose s1 s2))
  putStrLn ""

showDsTest :: Term -> Term -> IO()
showDsTest t1 t2 = do
  putStr ("\nds " ++ (pretty t1) ++ " & " ++ (pretty t2) ++ "\n  |  \n  V\n")
  if (show (ds t1 t2)) == "Nothing" 
    then do
      putStrLn "Nothing"
      putStrLn ""
  else do
    let (Just (t1',t2')) = ds t1 t2
    putStrLn ("(" ++ (pretty t1') ++ "," ++ (pretty t2') ++ ")")
    putStrLn ""

showUnifyTest :: Term -> Term -> IO()
showUnifyTest t1 t2 = do
  putStr ("\nunify " ++ (pretty t1) ++ " & " ++ (pretty t2) ++ "\n  |  \n  V\n")
  if (show (unify t1 t2)) == "Nothing" 
    then do
      putStrLn "Nothing -> nicht unifizierbar"
      putStrLn ""
  else do
    let (Just s) = unify t1 t2
    putStrLn (pretty s)
    putStrLn ""

showTitle :: String -> IO()
showTitle s = do
  putStr ("\n\n" ++ (take (20+(length s)) stars))
  putStr ("\n*         " ++ s ++ "         *")
  putStr ("\n" ++ (take (20+(length s)) stars) ++ "\n\n")

stars :: String
stars = '*':stars