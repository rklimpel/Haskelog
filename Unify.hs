module Unify where

import Type
import Pretty
import Sub
import Utils.TermUtils
import Test.Samples

-- berechnet die Unstimmigkeitsmenge zweier Terme 
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var x) (Var y)
    | x == y                                       = Nothing
    | otherwise                                    = Just (Var x,Var y)
ds (Var i) t                                       = Just (Var i,t)
ds t (Var i)                                       = Just (t, Var i)
ds (Comb s1 t1s) (Comb s2 t2s)
    -- wenn die Terme gleich sind
    | isTermEq (Comb s1 t1s) (Comb s2 t2s)         = Nothing
    -- wenn die länge der Terme nicht gleich ist oder die 'Funktion sich unterscheidet
    | length t1s /= length t2s || s1 /= s2         = Just ((Comb s1 t1s),(Comb s2 t2s))
    -- wenn die Länge der Terme gleich ist und die Funktion auch -> Step in
    | length t1s == length t2s && s1 == s2         = getDiffTermList t1s t2s

-- gibt das erste nicht übereinstimmende Tupel zurück
getDiffTermList :: [Term] -> [Term] -> Maybe (Term,Term)
getDiffTermList [t1] [t2] = ds t1 t2
getDiffTermList (t1:t1s) (t2:t2s)
    | isTermEq t1 t2         = getDiffTermList t1s t2s
    | otherwise              = ds t1 t2

-- bestimmt den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unify' t1 t2 0 (Subst [])

unify' ::Term -> Term -> Int -> Subst -> Maybe Subst
unify' t1 t2 k o
    -- wenn die Terme gleich sind wenn die Substitution angewendet wird
  --  | isTermEq (apply o t1) (apply o t2) = Just o   
    | otherwise                          = 
        case (ds (apply o t1) (apply o t2)) of
        Just ((Var x),g)    -> if checkVarInTerm x g == False 
                                then unify' t1 t2 (k+1) (compose (single x g) o)
                               else Nothing
        Just (g,(Var x))    -> unify' t2 t1 k o
        Just ((Comb _ _),g) -> Nothing
        Nothing             -> Just o