{-# LANGUAGE ViewPatterns #-}

module Interarctive where

import Pretty
import Type
import Data.List
import Parser
import Search

main :: IO()
main = do
    putStr "\nInteractive environment started...\n\n"
    putStr "Welcome to Simple Prolog!\n"
    putStr "Type \":help\" for help.\n"
    shell (Prog []) False dfs


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
        (stripPrefix ":load " -> Just file) -> do
            interpretFile file p treeActivated searchStrategy
        (stripPrefix ":set " -> Just strat) -> do
            interpretStategy strat p treeActivated searchStrategy
        goal -> do
            processGoal goal p treeActivated searchStrategy


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
    putStrLn ""
    putStrLn "To handle massive Solutions:"
    putStrLn "press <Enter> to show next possible Solution"
    putStrLn "type anything & press <Enter> to cancel Solution Output"

printInfo :: Prog -> IO()
printInfo (Prog rs) = do 
    putStr "Available predicates:\n"
    let predicates = (concat (intersperse "\n" (map getPredicate rs)))
    if predicates /= "" then putStr (predicates ++ "\n") else putStr (inRed ("No Predicates available.\n"))
   
getPredicate :: Rule -> String
getPredicate (Comb s ts :- _) = s ++ "/" ++ (show (length ts))
getPredicate _                = ""

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

processGoal :: String -> Prog -> Bool -> Strategy -> IO()
processGoal goal p treeActivated searchStrategy = case (parseWithVars goal) of
                                                    Left e -> do 
                                                        putStr (inRed ("ERROR: Undefined input: \"" ++ e ++ "\"\n"))
                                                        shell p treeActivated searchStrategy
                                                    Right ((Goal ts),realNames) -> do
                                                        putStr "\n"
                                                        printResult (solve searchStrategy p (Goal ts)) realNames
                                                        shell p treeActivated searchStrategy

printResult :: [Subst] -> [(VarIndex,String)] -> IO()
printResult [] realNames     = putStr "\n"
printResult (x:xs) realNames = do
    putStr ((prettyWithVars realNames x))
    input <- getLine
    case input of 
        "" -> printResult xs realNames
        otherwise -> do
            putStr "\n"
            return()

inRed :: String -> String
inRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

inGreen :: String -> String
inGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"