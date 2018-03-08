module Search (dfs,bfs,solve) where

import SLDTree
import Type
import Sub

import Utils.TermUtils
import Utils.SubUtils
import Utils.Queue

import Data.Maybe
import Data.List

{-

Remember:
Alias type for Search Strategys
type Strategy = SLDTree -> [Subst]

-}

-- PUBLIC FUNCTIONS

-- searches for All Solution Substituations in a SLD Tree with deep search
dfs :: Strategy
dfs sld = dfs' sld Sub.empty
    where 
    dfs' :: SLDTree -> Subst -> [Subst]
    -- Goal Empty -> found a solution
    dfs' (SLDTree (Goal []) []) s = [s]
    -- Goal not Emtpy, but no more Ledges -> no solution on this way
    dfs' (SLDTree _ []) _         = []
    -- Goal not Empty, more Ledges to go -> Step inside the trees leafes
    dfs' (SLDTree _ x) s          = concat (map (dfs'' s) x)
        where
        -- helper to apply the substitution to the exisiting one for every 'subtree' of the one you are looking at
        dfs'' :: Subst -> (Subst,SLDTree) -> [Subst]
        dfs'' sub2 (sub,tree) = dfs' tree (compose sub sub2)

-- searches for All Solution Substituations in a SLD Tree mit Breitensuche
bfs :: Strategy
bfs tree = bfs' [(Sub.empty,tree)] []
    where 
    bfs' :: Queue -> [Subst] -> [Subst]
    bfs' q subs = case getElement q of
        -- Tree is leaf, queue is not empty, goal is a result
        Just (que,(sub,SLDTree (Goal []) [])) -> bfs' que (subs ++ [sub])
        -- tree is leaf, queue is not empty, goal is no result
        Just (que,(sub,SLDTree _ []))         -> bfs' que subs
        -- Tree is not a leaf -> Add all further branches to the queue
        Just (que,(sub,SLDTree _ ts)) -> bfs' (foldl (\que (sub2,ts2) -> addElement que ((compose sub2 sub),ts2)) que ts) subs
        -- Queue is empty -> List of previous written substitutions is the result
        Nothing -> subs  

-- Solves a request + program. Returns found results as [noun]. Get the result list from a search strategy
solve :: Strategy -> Prog -> Goal -> [Subst]
solve searchStrategy p g = let result     = searchStrategy (sld p g)                 -- Result from Search Strategy, List of Subst
                               result'    = map (filterForGoalVars g) result         -- Result without replaces whose variable does not occur in the goal
                               result''   = nub result'                              -- Result without duplicate entries
                               -- resultEnd  = mapMaybe (subTermVarsInGoal g) result''     -- Result without substitution on non-goal variables
                            in result''

-- INTERNAL FUNCTIONS

-- deletes the replacements from the substitutions whose variables (left) are not in the goal
filterForGoalVars :: Goal -> Subst -> Subst
filterForGoalVars go s = filterForGoalVars' go s []
    where
    filterForGoalVars' :: Goal -> Subst -> [Replace] -> Subst
    filterForGoalVars' _ (Subst []) xs    = (Subst xs)
    filterForGoalVars' g (Subst ((Replace i t):rs)) xs 
        = let listUntilNow = xs
              checkHead    = fromMaybe [] (isElement i t (getVarsInGoal g))
              restList     = getReplacements (filterForGoalVars' g (Subst rs) xs)
          in Subst (listUntilNow ++ (checkHead ++ restList))

-- returns a substitution only if there are only constants on the right side
-- or the variables on the right appear in the goal-terms
subTermVarsInGoal :: Goal -> Subst -> Maybe Subst
subTermVarsInGoal (Goal ts) (Subst rs) 
    = if elem True (map (termListHasSubterm ts) (map getTerm rs)) 
        || getVarsInTermList (map getTerm rs) == [] 
        then Just (Subst rs) 
      else Nothing

-- Check if the variable of a replace is contained in a term
-- either returns Just the Replace or Nothing
isElement :: VarIndex -> Term -> [VarIndex] -> Maybe [Replace]
isElement x t ys = if elem x ys then Just [(Replace x t)] else Nothing

-- Returns a list of all the replacements in a substitution
getReplacements :: Subst -> [Replace]
getReplacements (Subst rs) = rs

-- returns all variables that occur in a goal
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts
