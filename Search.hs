module Search where

import SLDTree
import Type
import Sub
import Utils.TermUtils
import Utils.SubUtils


-- Alias type for Search Strategys
-- type Strategy = SLDTree -> [Subst]


dfs :: Strategy
dfs sld = dfs' sld Sub.empty

dfs' :: SLDTree -> Subst -> [Subst]
-- Goal Empty -> Lösung gefunden
dfs' (SLDTree (Goal []) []) s = [s]
-- Goal not Emtpy, but no more Ledges -> kein Lösung auf diesem Weg
dfs' (SLDTree _ []) _         = []
-- Goal note Empty, more Ledges to go -> Step in 
dfs' (SLDTree _ x) s          = concat (map (\(s2,st) -> dfs' st (compose s2 s)) x)  -- ? hmmmmm


{-
bfs :: Strategy
bfs (SLDTree g l) = bfs' sld Sub.empty l 

bfs' :: SLDTree -> Subst -> [(SLDTree,Subst)] -> [Subst]
-}


solve :: Strategy -> Prog -> Goal -> [Subst]
solve searchStrategy p g = searchStrategy (sld p g)

--filterForGoalVars :: [VarIndex]

-- gibt alle Varibalen zurück die in einer Anfrage vorkommen
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts
