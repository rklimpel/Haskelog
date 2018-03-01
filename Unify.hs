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
    | pretty (Comb s1 t1s) == pretty (Comb s2 t2s) = Nothing
    | length t1s /= length t2s || s1 /= s2         = Just ((Comb s1 t1s),(Comb s2 t2s))
    | length t1s == length t2s && s1 == s2         = getDiffTermList t1s t2s

-- gibt das erste nicht übereinstimmende Tupel zurück
getDiffTermList :: [Term] -> [Term] -> Maybe (Term,Term)
getDiffTermList [t1] [t2]
    | pretty t1 == pretty t2 = Nothing
    | otherwise              = Just (t1,t2)
getDiffTermList (t1:t1s) (t2:t2s)
    | pretty t1 == pretty t2 = getDiffTermList t1s t2s
    | otherwise              = getDiffTerm t1 t2

-- quick info -> geht bei den Termen in die tiefe...
getDiffTerm :: Term -> Term -> Maybe (Term,Term)
getDiffTerm (Comb s1 [t1]) (Comb s2 [t2])
    | s1 == s2    = getDiffTerm t1 t2
    | otherwise   = Just ((Comb s1 [t1]),(Comb s2 [t2]))

getDiffTerm (Comb s1 (t1:t1s)) (Comb s2 (t2:t2s))
    | s1 == s2    = getDiffTermList (t1:t1s) (t2:t2s)
    | otherwise   = Just ((Comb s1 (t1:t1s)),(Comb s2 (t2:t2s)))

-- bestimmt den allgemeinsten Unifikator für∫ zwei Terme, 
-- sofern die beiden Terme unifizierbar sind
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifySub t1 t2 (Subst [])

unifySub ::Term -> Term -> Subst -> Maybe Subst
unifySub t1 t2 o
    | pretty (apply o t1) == pretty (apply o t2) = Just o