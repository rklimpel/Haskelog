module SLDTree where

import Type
import Utils.TermUtils
import Sub

-- konstruiert den SLD-Baum zu einem Programm und einer Anfrage
-- Selektionsstrategie FIRST (es wird immer das linkeste Literal zum Beweisen ausgewählt)
sld :: Prog -> Goal -> SLDTree
sld p (Goal ts) = sldHelper (incVarsProg (maxVarInTermlist ts) p) (Goal ts) empty
    where
        sldHelper :: Prog -> Goal -> Subst -> SLDTree
        sldHelper p (Goal []) s = SLDTree (Goal []) []


-- Gibt die größte Variable in einer Regel zurück
maxVarInRule :: Rule -> Int
maxVarInRule  (l :- r) = maxVarInTermlist (l:r)

-- gibt die größte Variable in einer Liste von Termen zurück (0 wenn keine Variable enthalten ist)
maxVarInTermlist :: [Term] -> Int
maxVarInTermlist ts 
    | termListHasVar ts = maximum (map maxVarInTerm ts)
    | otherwise         = 0

-- gibt die größte Variable in einem Term zurück
maxVarInTerm:: Term -> Int
maxVarInTerm (Var v)     = v
maxVarInTerm (Comb s ts) 
    | termListHasVar ts  = maximum (map maxVarInTerm ts)
    | otherwise          = 0

-- erhöhe Variabelen in Programm um einen bestimmten Wert
incVarsProg :: Int -> Prog -> Prog
incVarsProg i (Prog [r])    = Prog [incVarsRule i r]
incVarsProg i (Prog (r:rs)) = Prog (incVarsRule i r:(getRuleListFromProg (incVarsProg (i+(maximum (getVarsInRule r))) (Prog rs))))

getRuleListFromProg :: Prog -> [Rule]
getRuleListFromProg (Prog rs) = rs

-- erhöhre Variablen in Regel um einen bestimmten Wert
incVarsRule :: Int -> Rule -> Rule
incVarsRule i (l :- rs) = incVarsTerm i l :- map (incVarsTerm i) rs

-- ehöhe Variablen in Term um einen bestimmten Wert
incVarsTerm :: Int -> Term -> Term
incVarsTerm i (Var x)    = Var (x+i)
incVarsTerm i (Comb s t) = Comb s (map (incVarsTerm i) t)

-- gibt eine Liste an Variablen zurück die in einem Term Vorkommen                             
getVarsInTerm :: Term -> [VarIndex]
getVarsInTerm (Var a)     = [a]
getVarsInTerm (Comb _ []) = []
getVarsInTerm (Comb _ ts) = getVarsInTermList ts

-- gibt alle Varibalen zurück die in einer Anfrage vorkommen
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts

-- gibt alle Variablen zurück die in einer Regel vorkommen
getVarsInRule :: Rule -> [Int]
getVarsInRule (rl :- rr) = getVarsInTermList ([rl] ++ rr)

-- gibt alle Variablen zurück die in einer Liste von Termn vorkommen
getVarsInTermList :: [Term] -> [Int]
getVarsInTermList []                 = []
getVarsInTermList ((Var x) : xs)     = [x] ++ (getVarsInTermList xs)
getVarsInTermList ((Comb _ xs) : ts) = (getVarsInTermList xs) ++ (getVarsInTermList ts)