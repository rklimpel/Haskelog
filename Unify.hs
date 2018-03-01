module Unify where

import Type
import Pretty
import Sub
import Utils.TermUtils

-- berechnet die Unstimmigkeitsmenge zweier Terme 
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var i) t                                       = Just (Var i,t)
ds t (Var i)                                       = Just (t, Var i)
ds (Comb s1 t1s) (Comb s2 t2s)
    | isTermEq (Comb s1 t1s) (Comb s2 t2s)         = Nothing
    | length t1s /= length t2s || s1 /= s2         = Just ((Comb s1 t1s),(Comb s2 t2s))
    | length t1s == length t2s && s1 == s2         = getDiffTermList t1s t2s

-- gibt das erste nicht übereinstimmende Tupel zurück
getDiffTermList :: [Term] -> [Term] -> Maybe (Term,Term)
getDiffTermList [t1] [t2] = ds t1 t2
getDiffTermList (t1:t1s) (t2:t2s)
    | isTermEq t1 t2         = getDiffTermList t1s t2s
    | otherwise              = ds t1 t2

-- bestimmt den allgemeinsten Unifikator für∫ zwei Terme, 
-- sofern die beiden Terme unifizierbar sind
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifySub t1 t2 (Subst [])

unifySub ::Term -> Term -> Subst -> Maybe Subst
unifySub t1 t2 o
    | isTermEq (apply o t1) (apply o t2) = Just o
    | otherwise                          = unifyHelper t1 t2 (ds (apply o t1) (apply o t2)) o

unifyHelper :: Term -> Term -> Maybe (Term,Term) -> Subst -> Maybe Subst
unifyHelper t1 t2 (Just ((Var x),ds2)) o 
    | termHasSubterm t2 t1 == False       = unifySub t1 t2 (compose (single x ds2) o)
    | otherwise                            = Nothing
unifyHelper _ _ (Just ((Comb s t),ds2)) _  = Nothing