module SLDTree (sld) where

import Type
import Sub
import Unify

import Utils.TermUtils

import Data.Maybe (catMaybes)

-- PUBLIC FUNCTIONS

-- constructs a SLD tree from a request and a program
-- Selection strategy: First
-- the left literal is always selected for proofing
sld :: Prog -> Goal -> SLDTree
sld p (Goal ts) = sldHelper (incVarsProg ((maxVarInTermlist ts)+1) p) (Goal ts) Sub.empty
    where
    sldHelper :: Prog -> Goal -> Subst -> SLDTree
    -- Goal is Empty -> returns SLD Tree without "leaves"
    sldHelper p (Goal []) s = SLDTree (Goal []) []
    -- Goal not Empty -> returns SLD Tree with leaves that have been processed
    -- empty leaves will be removed through the filter
    sldHelper (Prog rs) g s = SLDTree g (catMaybes (map (sldHelper' (Prog rs) g s) rs))

    -- guckt nach ob 
    sldHelper' :: Prog -> Goal -> Subst -> Rule -> Maybe (Subst, SLDTree)
    sldHelper' p (Goal ts) s (rh :- rt) = 
        case (unify (head ts) rh) of
        -- if Unify has found a substitution then go deeper into the tree
        -- return: su: the unification of the head of goal and the rule found by unify
        Just su -> let compSub    = compose s su
                       progOffset = incVarsProg ((subGoalMaxVarIndex su (Goal ts))+1) p
                       newGoal    = Goal (map (apply su) (rt ++ (tail ts)))
                       newTree    = sldHelper progOffset newGoal compSub
                    in Just (su,newTree)
        -- wenn Unify Nothing zurückgibt ist bricht der SLDTree an dieser Stelle ab
        Nothing -> Nothing

-- INTERNAL FUNCTIONS

-- returns the largest variable in a rule
maxVarInRule :: Rule -> VarIndex
maxVarInRule  (l :- r) = maxVarInTermlist (l:r)

-- returns the largest variable in a substitution
maxVarInSubst :: Subst -> VarIndex
maxVarInSubst (Subst []) = 0
maxVarInSubst (Subst rs) = maxVarInReplaceList rs

-- Returns the largest variable of a list of replaces (single subst)
maxVarInReplaceList :: [Replace] -> VarIndex
maxVarInReplaceList [] = 0
maxVarInReplaceList rs = let x = maximum (map getVarOfReplace rs)
                             y = maxVarInTermlist (map getTermOfReplace rs)
                        in if x >= y then x else y

-- returns the largest variable of substitution and goal
subGoalMaxVarIndex :: Subst -> Goal -> VarIndex
subGoalMaxVarIndex s (Goal ts) = maximum ((maxVarInSubst s):[(maxVarInTermlist ts)])

-- returns the variable from the Replace data type
getVarOfReplace :: Replace -> VarIndex
getVarOfReplace (Replace v _) = v

-- returns the term from the Replace data type
getTermOfReplace :: Replace -> Term
getTermOfReplace (Replace _ t) = t
    
-- increase all variables in program by a certain value i
incVarsProg :: Int -> Prog -> Prog
incVarsProg _ (Prog [])     = Prog []
incVarsProg i (Prog [r])    = Prog [incVarsRule i r]
incVarsProg i (Prog (r:rs)) = Prog (incVarsRule i r:(getRuleListFromProg (incVarsProg i (Prog rs))))

-- returns the list of all rules from a prog
getRuleListFromProg :: Prog -> [Rule]
getRuleListFromProg (Prog rs) = rs

-- increase all variables in a rule by a certain amount
incVarsRule :: Int -> Rule -> Rule
incVarsRule i (l :- rs) = incVarsTerm i l :- map (incVarsTerm i) rs

-- increase all variables in term by a certain value
incVarsTerm :: Int -> Term -> Term
incVarsTerm i (Var x)    = Var (x+i)
incVarsTerm i (Comb s t) = Comb s (map (incVarsTerm i) t)

-- returns all variables that occur in a goal
getVarsInGoal :: Goal -> [VarIndex]
getVarsInGoal (Goal ts) = getVarsInTermList ts

-- returns all variables that occur in a rule
getVarsInRule :: Rule -> [VarIndex]
getVarsInRule (rl :- rr) = getVarsInTermList ([rl] ++ rr)