{-# LANGUAGE ViewPatterns #-}

module Interactive (main) where

import Pretty
import Type
import Parser
import Search
import SLDTree

import Data.List

-- PUBLIC FUNCTIONS

-- starts the Interactive Prolog environment
main :: IO()
main = do
    putStr "\nInteractive environment started...\n\n"
    putStr "Welcome to Simple Prolog!\n"
    putStr "Type \":help\" for help.\n"
    shell (Prog []) False dfs

-- INTERNAL FUNCTIONS

-- processes a single input in the Interactive Prolog environment
shell :: Prog -> Bool -> Strategy -> IO()
shell p treeActivated searchStrategy = do
    putStr "?- "
    input <- getLine
    case input of 
        ":help" -> do
            printHelp
            shell p treeActivated searchStrategy
        ":quit" -> do
            putStr "Halt!\n"
            return()
        ":q"    -> do
            putStr "Halt!\n"
            return()
        ":info" -> do
            printInfo p
            shell p treeActivated searchStrategy
        ":showProg" -> do
            printProg p
            shell p treeActivated searchStrategy
        ":toggleTree" -> do
            if treeActivated
                then do
                    putStr "show SLDTree turned OFF\n"
                    shell p False searchStrategy
                else do
                    putStr "show SLDTree turned ON\n"
                    shell p True searchStrategy
        (stripPrefix ":load " -> Just file) -> do
            interpretFile file p treeActivated searchStrategy
        (stripPrefix ":set " -> Just strat) -> do
            interpretStategy strat p treeActivated searchStrategy
        goal -> do
            processGoal goal p treeActivated searchStrategy

-- displays the help
printHelp :: IO()
printHelp = do
    putStrLn "Commands available from the prompt:"
    putStrLn "<goal>        Solves/proves the specified goal."
    putStrLn ":help         Shows this help message."
    putStrLn ":info         Shows all available predicates."
    putStrLn ":load <file>  Loads the specified file."
    putStrLn ":quit         Exits the interactive environment."
    putStrLn ":set <strat>  Sets the specified search strategy"
    putStrLn "              where <strat> is one of 'dfs' or 'bfs'."
    putStrLn ":toggleTree   Toggles if SLDTree is shown when processing"
    putStrLn "              goal or not."
    putStrLn ":printProg    print the whole loaded Programm in Prolog Syntax"
    putStrLn ""
    putStrLn "To handle massive Solutions:"
    putStrLn "press <Enter> to show next possible Solution"
    putStrLn "type anything & press <Enter> to cancel Solution Output"

-- shows all predicates from the loaded Prolog file and how many arguments they have
printInfo :: Prog -> IO()
printInfo (Prog rs) = do 
    putStr "Available predicates:\n"
    let predicates = (concat (intersperse "\n" (map getPredicate rs)))
    if predicates /= "" then putStr (predicates ++ "\n") else putStr (inRed ("No Predicates available.\n"))

-- extrahiert Prädikate und Anzahl der Argumente aus einer Regel
getPredicate :: Rule -> String
getPredicate (Comb s ts :- _) = s ++ "/" ++ (show (length ts))
getPredicate _                = ""

-- extracts predicates and number of arguments from a rule
interpretFile :: String -> Prog -> Bool -> Strategy -> IO()
interpretFile file oldProg treeActivated searchStrategy = do
    fileContent <- parseFile file
    case fileContent of 
        Left e -> do 
            putStrLn (inRed ("ERROR: Invalid file: " ++ e))
            shell oldProg treeActivated searchStrategy
        Right p -> do
            putStrLn (inGreen ("Loaded file '" ++ file ++ "'."))
            shell p treeActivated searchStrategy

-- Interprets the user input for the SearchStrategy change
interpretStategy :: String -> Prog -> Bool -> Strategy -> IO()
interpretStategy newStrat p treeActivated oldStrat 
            | newStrat == "bfs" = do
                putStr "Strategy set to breadth-first search.\n"
                shell p treeActivated bfs
            | newStrat == "dfs" = do
                putStr "Strategy set to depth-first search.\n"
                shell p treeActivated dfs
            | otherwise         = do
                putStr (inRed ("ERROR: unkown search strategy: " ++ newStrat ++ "\n"))
                putStr "available strategys are: dfs,bfs\n"
                shell p treeActivated oldStrat

-- processes all other user input (assumption: it is a Goal)
processGoal :: String -> Prog -> Bool -> Strategy -> IO()
processGoal goal p treeActivated searchStrategy 
    = case (parseWithVars goal) of
        Left e -> do 
            putStr (inRed ("ERROR: Undefined input: \"" ++ e ++ "\"\n"))
            shell p treeActivated searchStrategy
        Right ((Goal ts),realNames) -> do
            putStr "\n"
            if treeActivated 
                then printSLDTree (sld p (Goal ts)) else putStr ""
            printResult (removeAnonymous (solve searchStrategy p (Goal ts)) realNames) realNames
            shell p treeActivated searchStrategy

-- removes anonymous Variables from the result substitutions
removeAnonymous :: [Subst] -> [(VarIndex,String)] -> [Subst]
removeAnonymous [] realNames                = []
removeAnonymous ((Subst rs):subs) realNames = [(removeAnonymous' rs realNames [])] ++ (removeAnonymous subs realNames)
    where
    -- removes all Replace's with anonymous Variables from a list of Replace's
    removeAnonymous' :: [Replace] -> [(VarIndex,String)] -> [Replace] -> Subst
    removeAnonymous' [] _ result = Subst result
    removeAnonymous' ((Replace i t):rs) realNames result 
        = case lookup i realNames of
            Just "_" -> removeAnonymous' rs realNames result
            _        -> removeAnonymous' rs realNames (result ++ [(Replace i t)])

-- shows the results of a request to the user (next solution with enter)
printResult :: [Subst] -> [(VarIndex,String)] -> IO()
printResult [] _     = putStr "\n"
printResult (x:xs) realNames = do
    putStr ((prettyWithVars realNames x))
    input <- getLine
    case input of 
        ""        -> printResult xs realNames
        _         -> putStr "\n"

-- displays an sld tree
printSLDTree :: SLDTree -> IO()
printSLDTree tree = do
    putStr "SLDTree:\n"
    putStr ""
    putStr (pretty tree)
    putStr "\n"

-- displays all rules of a program in Prolog Syntax
printProg :: Prog -> IO()
printProg p = do
    putStr ((pretty p) ++ "\n")

-- colors the text in the output red
inRed :: String -> String
inRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

-- colors the text in the output grün
inGreen :: String -> String
inGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"