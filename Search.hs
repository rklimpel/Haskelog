module Search where

import SLDTree
import Type
import Sub
import Utils.TermUtils
import Utils.SubUtils
import Data.Maybe
import Utils.Queue


-- Remember: Alias type for Search Strategys
-- type Strategy = SLDTree -> [Subst]


-- searches for All Solution Substituations in a SLD Tree with deep search
dfs :: Strategy
dfs sld = dfs' sld Sub.empty
    where 
    dfs' :: SLDTree -> Subst -> [Subst]
    -- Goal Empty -> Lösung gefunden
    dfs' (SLDTree (Goal []) []) s = [s]
    -- Goal not Emtpy, but no more Ledges -> kein Lösung auf diesem Weg
    dfs' (SLDTree _ []) _         = []
    -- Goal note Empty, more Ledges to go -> Step in 
    dfs' (SLDTree _ x) s          = concat (map (dfs'' s) x)
        where
        -- Step deeper inside the Tree....
        dfs'' :: Subst -> (Subst,SLDTree) -> [Subst]
        dfs'' sub2 (sub,tree) = dfs' tree (compose sub sub2)


bfs :: Strategy
bfs tree = bfs' [(Sub.empty,tree)] []
    where 
    bfs' :: Queue -> [Subst] -> [Subst]
    bfs' q subs = case getElement q of
        -- Baum ist Blatt, Warteschlange ist nicht leer
        Just (que,(sub,SLDTree g [])) -> bfs' que (subs ++ [sub])
        -- Baum ist kein Blatt
        Just (que,(sub,SLDTree _ ts)) -> bfs' (foldl (\que (sub2,ts2) -> addElement que ((compose sub2 sub),ts2)) que ts) subs
        --Just (que,(sub,SLDTree _ ts)) -> bfs' (foldl (bfs'' que sub) que ts) subs
        -- Warteschlange ist leer
        Nothing -> subs
        where 
        bfs'' :: Queue -> Subst -> (Subst,SLDTree) -> Queue
        bfs'' q sub (s,st) = addElement q (compose s sub,st)

 

 --bfs' :: SLDTree -> Subst -> [(SLDTree,Subst)] -> [Subst]
-- bfs' _ _ _ = [Subst []]



solve :: Strategy -> Prog -> Goal -> [Subst]
solve searchStrategy p g = map (filterForGoalVars g) (searchStrategy (sld p g))

filterForGoalVars :: Goal -> Subst -> Subst
filterForGoalVars g s = filterForGoalVars' g s []

filterForGoalVars' :: Goal -> Subst -> [Replace] -> Subst
filterForGoalVars' g (Subst []) xs    = (Subst xs)
filterForGoalVars' g (Subst ((Replace i t):rs)) xs 
    = Subst (xs ++ (fromMaybe [] (isElement i t (getVarsInGoal g)) ++ (getReplacements (filterForGoalVars' g (Subst rs) xs))))

isElement :: VarIndex -> Term -> [VarIndex] -> Maybe [Replace]
isElement x t ys = if elem x ys then Just [(Replace x t)] else Nothing

getReplacements :: Subst -> [Replace]
getReplacements (Subst rs) = rs

-- gibt alle Varibalen zurück die in einer Anfrage vorkommen
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts
