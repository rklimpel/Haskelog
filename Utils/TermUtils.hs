module Utils.TermUtils where

import Type
import Pretty

-- True if given Term is Var
isVar :: Term -> Bool
isVar (Var i)    = True
isVar (Comb s t) = False

-- check if Term1 contains Term2 (Term 2 is a subterm of Term1)
containsSubterm :: Term -> Term -> Bool
containsSubterm (Var x) (Var y)
    | x == y                            = True
    | otherwise                         = False
containsSubterm (Var x) (Comb s t)      = False
containsSubterm (Comb s []) t
    | pretty (Comb s []) == pretty t    = True
    | otherwise                         = False
containsSubterm (Comb s [t1]) t2
    | pretty (Comb s [t1]) == pretty t2 = True
    | otherwise                         = containsSubterm t1 t2
containsSubterm (Comb s (t1:t1s)) t2
    | pretty (Comb s (t1:t1s)) == pretty t2 = True
    | otherwise                             = containsSubtermL t1s t2

-- check if a list of Terms contains a subterm
containsSubtermL :: [Term] -> Term -> Bool
containsSubtermL [t1] t2            = containsSubterm t1 t2
containsSubtermL (t1:t1s) t2
    | containsSubterm t1 t2 == True = True
    | otherwise                     = containsSubtermL t1s t2