module Unify where

import Type
import Pretty

-- berechnet die Unstimmigkeitsmenge zweier Terme 
ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 
    | pretty t1 == pretty t2 = Nothing
    | isVar t1 || isVar t2   = Just (t1,t2)


unify :: Term -> Term -> Maybe Subst
unify _ _ = Nothing

isVar :: Term -> Bool
isVar (Var i)    = True
isVar (Comb s t) = False