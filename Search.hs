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

-- DEPRECATED, why?
-- searches for All Solution Substituations in a SLD Tree bfs
{-
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
        Just (que,(sub,SLDTree _ ts))         -> bfs' (addKidsToQueue que sub ts) subs
            where
            addKidsToQueue :: Queue -> Subst -> [(Subst,SLDTree)] -> Queue
            addKidsToQueue qu rootSub []                    = qu
            addKidsToQueue qu rootSub ((newSub,newTree):xs) = let kid = ((compose newSub rootSub),newTree)
                                                              in addKidsToQueue (addElement qu kid) rootSub xs
        -- Queue is empty -> List of previous written substitutions is the result
        Nothing -> subs  
-}



{-
bfs :: Strategy
bfs tree@(SLDTree (Goal g) cs) = bfs' tree Sub.empty [(tree,Sub.empty)]
    where
    bfs' :: SLDTree -> Subst -> [(SLDTree,Subst)] -> [Subst]
    bfs' (SLDTree (Goal []) []) rootSub ((t,s):[]) = [s]
    bfs' (SLDTree (Goal _) []) rootSub ((t,s):[])  = []
    bfs' (SLDTree (Goal []) []) rootSub ((t,s):(t2,s2):xs) = [s] ++ (bfs' t2 s2 ((t2,s2):xs))
    bfs' (SLDTree (Goal _) []) rootSub ((t,s):(t2,s2):xs)  = [] ++ (bfs' t2 s2 ((t2,s2):xs))
    bfs' (SLDTree (Goal _) subTrees) rootSub queue              = let newQueue = (fillQueue subTrees (tail queue) rootSub)
                                                             in bfs' (fst (head newQueue)) (snd (head newQueue)) newQueue
        where
        fillQueue :: [(Subst,SLDTree)] -> [(SLDTree,Subst)] -> Subst -> [(SLDTree,Subst)]
        fillQueue [] tree _ = tree
        fillQueue ((sub,t@(SLDTree (Goal _) xs)):ts) tree rootSub 
            = fillQueue ts (tree ++ [(t,compose sub rootSub)]) rootSub
-}

type Queue = [(Subst,SLDTree)]

bfs :: Strategy
bfs tree@(SLDTree (Goal g) cs) = bfs' [(Sub.empty,tree)]
    where
    bfs' :: Queue -> [Subst]
    bfs' ((sub,t@(SLDTree (Goal []) [])):[])      = [sub]
    bfs' ((sub,t@(SLDTree (Goal _)  [])):[])      = []
    bfs' ((sub,t@(SLDTree (Goal []) [])):xs)      = [sub] ++ (bfs' xs)
    bfs' ((sub,t@(SLDTree (Goal _)  [])):xs)      = [] ++ (bfs' xs)
    bfs' ((sub,t@(SLDTree (Goal _) subTrees)):xs) = let newQueue = (addToQueue subTrees xs sub)
                                                    in bfs' newQueue
        where
        addToQueue :: [(Subst,SLDTree)] -> Queue -> Subst -> Queue
        addToQueue [] que _ = que
        addToQueue ((sub,t@(SLDTree (Goal _) xs)):ts) que rootSub 
            = fillQueue ts (que ++ [(compose sub rootSub,t)]) rootSub
            

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
