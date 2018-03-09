module Search (dfs,bfs,solve) where

import SLDTree
import Type
import Sub

import Utils.TermUtils
import Utils.SubUtils

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

-- Datatype for Queue of Subs & SLDTrees for BFS
type Queue = [(Subst,SLDTree)]

-- searches for All Solution Substituations in a SLD Tree bfs
bfs :: Strategy
bfs tree@(SLDTree (Goal g) cs) = bfs' [(Sub.empty,tree)]
    where
    bfs' :: Queue -> [Subst]
    -- tree hast no subtrees, first element of the queue is a result, queue has NO further elements
    bfs' ((sub,t@(SLDTree (Goal []) [])):[])      = [sub]
    -- tree hast no subtrees, first element of the queue is NO result, queue has NO further elements
    bfs' ((sub,t@(SLDTree (Goal _)  [])):[])      = []
    -- tree hast no subtrees, first element of the queue is a result, queue contains further elements
    bfs' ((sub,t@(SLDTree (Goal []) [])):xs)      = [sub] ++ (bfs' xs)
    -- tree hast no subtrees, first element of the queue is NO result, queue contains further elements
    bfs' ((sub,t@(SLDTree (Goal _)  [])):xs)      = [] ++ (bfs' xs)
    -- tree hast no subtrees, can not be a result -> add subtrees in queue
    bfs' ((sub,t@(SLDTree (Goal _) subTrees)):xs) = let newQueue = (addToQueue subTrees xs sub)
                                                    in bfs' newQueue
        where
        -- adds a List of Subtrees to the Queue
        addToQueue :: [(Subst,SLDTree)] -> Queue -> Subst -> Queue
        addToQueue [] que _ = que
        addToQueue ((sub,t@(SLDTree (Goal _) xs)):ts) que rootSub 
            = addToQueue ts (que ++ [(compose sub rootSub,t)]) rootSub
            

-- Solves a request + program. Returns found results as [noun]. Get the result list from a search strategy
-- handles result 'filtering'
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
isElement x t ys = if elem x ys 
                    then Just [(Replace x t)] 
                    else Nothing

-- returns all variables that occur in a goal
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts
