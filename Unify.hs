module Unify (ds,unify) where

import Type
import Sub

import Utils.TermUtils

-- PUBLIC FUNTIONS

-- calculates the disagreement set of to terms
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var x) (Var y)
    | x == y                                       = Nothing
    | otherwise                                    = Just (Var x,Var y)
ds (Var i) t                                       = Just (Var i,t)
ds t (Var i)                                       = Just (t, Var i)
ds (Comb s1 t1s) (Comb s2 t2s)
    -- if the terms are equal
    | isTermEq (Comb s1 t1s) (Comb s2 t2s)         = Nothing
    -- if the length of the terms is not the same or the function is different
    | length t1s /= length t2s || s1 /= s2         = Just ((Comb s1 t1s),(Comb s2 t2s))
    -- if the length of the terms is the same and the function also -> Step inside the termlist
    | length t1s == length t2s && s1 == s2         = getDiffTermList t1s t2s
    where
    -- returns the first mismatched tuple (from left to right)
    getDiffTermList :: [Term] -> [Term] -> Maybe (Term,Term)
    getDiffTermList [t1] [t2]    = ds t1 t2
    getDiffTermList (t1:t1s) (t2:t2s)
        | isTermEq t1 t2         = getDiffTermList t1s t2s
        | otherwise              = ds t1 t2

-- calculates the most general unifier of two terms
-- returns Nothing if the terms are not unifiable
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unify' t1 t2 (Subst [])
    where
    unify' ::Term -> Term -> Subst -> Maybe Subst
    unify' t1 t2  o = 
            case (ds (apply o t1) (apply o t2)) of
            Just ((Var x),g)             -> if checkVarInTerm x g == False 
                                              then unify' t1 t2 (compose (single x g) o)
                                              else Nothing
            Just (g,(Var x))             -> if checkVarInTerm x g == False 
                                              then unify' t1 t2 (compose (single x g) o)
                                              else Nothing
            -- fail, a whole term can not be converted appropriately
            Just ((Comb _ _),(Comb _ _)) -> Nothing
            -- Terms have no ds; are equal when applied substitution
            Nothing                      -> Just o
