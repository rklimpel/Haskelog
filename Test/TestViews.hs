module Test.TestViews where

import Pretty
import Type
import Sub
import Unify
import SLDTree

-- Test Show Helper:

showPrettyTest :: Show a => Pretty a => a -> IO()
showPrettyTest a = do
  putStr ("\n pretty (" ++ (show a) ++ ") \n  |  \n  V\n")
  putStrLn (inGreen (pretty a))
  putStrLn ""

showApplySubstTest :: Subst -> Term -> IO()
showApplySubstTest s t = do
  putStr ("\napply " ++ (pretty s) ++ " on " ++ (pretty t) ++ "\n  |  \n  V\n")
  putStrLn (inGreen (pretty (apply s t)))
  putStrLn ""

showComposeSubstTest :: Subst -> Subst -> IO()
showComposeSubstTest s1 s2 = do
  putStr ("\n" ++ (pretty s1) ++ " o " ++ (pretty s2) ++ "\n  |  \n  V\n")
  putStrLn (inGreen (pretty (compose s1 s2)))
  putStrLn ""

showDsTest :: Term -> Term -> IO()
showDsTest t1 t2 = do
  putStr ("\nds " ++ (pretty t1) ++ " & " ++ (pretty t2) ++ "\n  |  \n  V\n")
  if (show (ds t1 t2)) == "Nothing" 
    then do
      putStrLn (inGreen " -> no disagreement set")
      putStrLn ""
  else do
    let (Just (t1',t2')) = ds t1 t2
    putStrLn (inGreen ("(" ++ (pretty t1') ++ "," ++ (pretty t2') ++ ")"))
    putStrLn ""

showUnifyTest :: Term -> Term -> IO()
showUnifyTest t1 t2 = do
  putStr ("\nunify " ++ (pretty t1) ++ " & " ++ (pretty t2) ++ "\n  |  \n  V\n")
  if (show (unify t1 t2)) == "Nothing" 
    then do
      putStrLn (inGreen "Nothing -> nicht unifizierbar")
      putStrLn ""
  else do
    let (Just s) = unify t1 t2
    putStrLn (inGreen (pretty s))
    putStrLn ""

showSldTest :: Prog -> Goal -> IO()
showSldTest p g = do
    putStr ("\n" ++ (take 20 manyLines) ++ (buildSubtitle "Rules:") ++ "\n" ++ (pretty p))
    putStr ("\n"++ (buildSubtitle "Goal:") ++ (pretty g) ++ "\n" ++ (take 20 manyLines) ++ "\n         |  \n         V\n")
    putStrLn (inGreen (pretty (sld p g)))
    putStr ""

showTitle :: String -> IO()
showTitle s = do
  putStr ("\n\n" ++ (take (20+(length s)) stars))
  putStr ("\n*         " ++ s ++ "         *")
  putStr ("\n" ++ (take (20+(length s)) stars) ++ "\n\n")

showSubtitle :: String -> IO()
showSubtitle s = do
  putStr (inYellow ("\n::" ++ s ++ "\n"))

buildSubtitle :: String -> String
buildSubtitle s = (inYellow ("\n::" ++ s ++ "\n"))

stars :: String
stars = '*':stars

manyLines :: String
manyLines = '-':manyLines

inGreen :: String -> String
inGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

inYellow :: String -> String
inYellow s = "\x1b[33m"++ s ++ "\x1b[0m"