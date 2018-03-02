module SLDTree (sld) where

import Type

-- konstruiert den SLD-Baum zu einem Programm und einer Anfrage
-- Selektionsstrategie FIRST (es wird immer das linkeste Literal zum Beweisen ausgewählt)
sld :: Prog -> Goal -> SLDTree
sld _ _ = SLDTree (Goal []) []




-- gibt eine Liste an Variablen zurück die in einem Term Vorkommen                             
getVarsInTerm :: Term -> [VarIndex]
getVarsInTerm (Var a)     = [a]
getVarsInTerm (Comb _ []) = []
getVarsInTerm (Comb _ ts) = foldr (++) [] (map getVarIndex ts)

-- gibt alle Varibalen zurück die in einer Anfrage vorkommen
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal gs) = nub (varsInTermList gs)

-- gibt alle Variablen zurück die in einer Regel vorkommen
getVarsInRule :: Rule -> [Int]
getVarsInRule (rl :- rr) = varsInTermList ([rl] ++ rr)

-- gibt alle Variablen zurück die in einer Liste von Termn vorkommen
getVarsinTermlist :: [Term] -> [Int]
getVarsinTermlist []                 = []
getVarsinTermlist ((Var x) : xs)     = [x] ++ (varsInTermList xs)
getVarsinTermlist ((Comb _ xs) : ts) = (varsInTermList xs) ++ (varsInTermList ts)