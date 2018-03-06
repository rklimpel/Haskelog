module SLDTree (sld) where

import Type
import Sub
import Unify

import Utils.TermUtils

import Data.Maybe (catMaybes)

-- PUBLIC FUNCTIONS

-- konstruiert den SLD-Baum zu einem Programm und einer Anfrage
-- Selektionsstrategie FIRST (es wird immer das linkeste Literal zum Beweisen ausgewählt)
sld :: Prog -> Goal -> SLDTree
sld p (Goal ts) = sldHelper (incVarsProg ((maxVarInTermlist ts)+1) p) (Goal ts) Sub.empty
    where
    sldHelper :: Prog -> Goal -> Subst -> SLDTree
    -- Goal ist Empty -> gibt SLD Tree ohne "Blätter" zurück
    sldHelper p (Goal []) s = SLDTree (Goal []) []
    -- Goal not Empty -> gibt SLD Tree mit Blättern zurück die weiterverarbeitet wurden
    -- leere Blätter werden durch den Filter entfernt
    sldHelper (Prog rs) g s = SLDTree g (catMaybes (map (sldHelper' (Prog rs) g s) rs))

    -- guckt nach ob 
    sldHelper' :: Prog -> Goal -> Subst -> Rule -> Maybe (Subst, SLDTree)
    sldHelper' p (Goal (g:gs)) s (rh :- rt) = 
        case (unify g rh) of
        -- wenn Unify eine Substitutuion gefunden hat dann steppe tiefer in den Boum rein
        -- Rückgabe: su : die von unify gefundene Unifizierung von dem head von goal und der Regel
        Just su -> let compSub    = compose s su
                       progOffset = incVarsProg ((subGoalMaxVarIndex su (Goal (g:gs)))+1) p     -- ? hmmm
                       newGoal    = Goal (map (apply su) (rt ++ gs))                            -- ? hmmm
                       newTree    = sldHelper progOffset newGoal compSub
                    in Just (su,newTree)
        -- wenn Unify Nothing zurückgibt ist bricht der SLDTree an dieser Stelle ab
        Nothing -> Nothing

-- INTERNAL FUNCTIONS

-- Gibt die größte Variable in einer Regel zurück
maxVarInRule :: Rule -> VarIndex
maxVarInRule  (l :- r) = maxVarInTermlist (l:r)

-- gibt die größte Variable in einer Substitution zurück
maxVarInSubst :: Subst -> VarIndex
maxVarInSubst (Subst []) = 0
maxVarInSubst (Subst rs) = maxVarInReplaceList rs

-- gibt die größte Variable eine Liste an Replaces (einzel Subst) zurück
maxVarInReplaceList :: [Replace] -> VarIndex
maxVarInReplaceList [] = 0
maxVarInReplaceList rs = let x = maximum (map getVarOfReplace rs)
                             y = maxVarInTermlist (map getTermOfReplace rs)
                        in if x >= y then x else y

-- gibt die größte Variable aus Substitution und Goal zurück
subGoalMaxVarIndex :: Subst -> Goal -> VarIndex
subGoalMaxVarIndex s (Goal ts) = maximum ((maxVarInSubst s):[(maxVarInTermlist ts)])

-- Gibt die Variable aus dem Replace Datentypen zurück
getVarOfReplace :: Replace -> VarIndex
getVarOfReplace (Replace v t) = v

-- Gibt den Term aus dem Replace Datentypen zurück
getTermOfReplace :: Replace -> Term
getTermOfReplace (Replace v t) = t
    
-- erhöhe Variabelen in Programm um einen bestimmten Wert
incVarsProg :: Int -> Prog -> Prog
incVarsProg i (Prog [])     = Prog []
incVarsProg i (Prog [r])    = Prog [incVarsRule i r]
incVarsProg i (Prog (r:rs)) = Prog (incVarsRule i r:(getRuleListFromProg (incVarsProg i (Prog rs))))

-- gibt die Liste aller Regeln aus einem Prog zurück
getRuleListFromProg :: Prog -> [Rule]
getRuleListFromProg (Prog rs) = rs

-- erhöhre Variablen in Regel um einen bestimmten Wert
incVarsRule :: Int -> Rule -> Rule
incVarsRule i (l :- rs) = incVarsTerm i l :- map (incVarsTerm i) rs

-- ehöhe Variablen in Term um einen bestimmten Wert
incVarsTerm :: Int -> Term -> Term
incVarsTerm i (Var x)    = Var (x+i)
incVarsTerm i (Comb s t) = Comb s (map (incVarsTerm i) t)

-- gibt alle Varibalen zurück die in einer Anfrage vorkommen
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts

-- gibt alle Variablen zurück die in einer Regel vorkommen
getVarsInRule :: Rule -> [VarIndex]
getVarsInRule (rl :- rr) = getVarsInTermList ([rl] ++ rr)