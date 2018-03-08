module Utils.TermUtils where

import Type
import Pretty

-- True if given Term is a variable
isVar :: Term -> Bool
isVar (Var i)    = True
isVar (Comb s t) = False

-- returns true if the one variable with index x is in the term
checkVarInTerm :: Int -> Term -> Bool
checkVarInTerm x (Var y)
    | x == y    = True
    | otherwise = False
checkVarInTerm x (Comb _ ts) = elem True (map (checkVarInTerm x) ts)

-- returns true if two terms are exactly the same
isTermEq :: Term -> Term -> Bool
isTermEq (Var x) (Var y) 
    | x == y                    = True
    | otherwise                 = False
isTermEq (Var x) (Comb s ts) = False
isTermEq (Comb s ts) (Var x) = False
isTermEq (Comb s1 t1s) (Comb s2 t2s)
    | s1 == s2                  = isTermListEq t1s t2s
    | otherwise                 = False

-- returns true if all terms of a list of terms are exactly the same
isTermListEq :: [Term] -> [Term] -> Bool
isTermListEq [] []             = True
isTermListEq [t1] [t2]         = isTermEq t1 t2
isTermListEq [t1] (t2:t2s)     = False
isTermListEq (t1:t1s) [t2]     = False
isTermListEq (t1:t1s) (t2:t2s)
    | isTermEq t1 t2 == True   = isTermListEq t1s t2s
    | otherwise                   = False
isTermListEq [] t              = False
isTermListEq t []              = False

-- check if Term1 contains Term2 (Term 2 is a subterm of Term1)
termHasSubterm :: Term -> Term -> Bool
termHasSubterm (Var x) (Var y)
    | x == y                           = True
    | otherwise                        = False
termHasSubterm (Var x) (Comb s t)      = False
termHasSubterm (Comb s []) t
    | pretty (Comb s []) == pretty t    = True
    | otherwise                         = False
termHasSubterm (Comb s t1) t2
    | pretty (Comb s t1) == pretty t2   = True
    | otherwise                         = termListHasSubterm t1 t2

-- check if a list of Terms contains a subterm
termListHasSubterm :: [Term] -> Term -> Bool
termListHasSubterm [] _             = False
termListHasSubterm [t1] t2          = termHasSubterm t1 t2
termListHasSubterm (t1:t1s) t2
    | termHasSubterm t1 t2 == True  = True
    | otherwise                     = termListHasSubterm t1s t2

-- returns true if there is a variable in the term
termHasVar :: Term -> Bool
termHasVar (Var _)     = True
termHasVar (Comb _ []) = False  
termHasVar (Comb s t)  = termListHasVar t

-- returns true if there is a variable in a list of terms
termListHasVar :: [Term] -> Bool
termListHasVar []  = False
termListHasVar [t] = termHasVar t
termListHasVar (t:ts)
    | termHasVar t = True
    | otherwise    = termListHasVar ts

-- returns all variables that occur in a term as a list of variable indexes                           
getVarsInTerm :: Term -> [VarIndex]
getVarsInTerm (Var a)     = [a]
getVarsInTerm (Comb _ []) = []
getVarsInTerm (Comb _ ts) = getVarsInTermList ts

-- returns all variables that appear in a list of terms as a list of variable indexes
getVarsInTermList :: [Term] -> [VarIndex]
getVarsInTermList []                 = []
getVarsInTermList ((Var x) : xs)     = [x] ++ (getVarsInTermList xs)
getVarsInTermList ((Comb _ xs) : ts) = (getVarsInTermList xs) ++ (getVarsInTermList ts)

-- returns the largest variable in a list of terms (0 if no variable is included)
maxVarInTermlist :: [Term] -> VarIndex
maxVarInTermlist []     = 0
maxVarInTermlist ts 
    | termListHasVar ts = maximum (map maxVarInTerm ts)
    | otherwise         = 0

-- returns the largest variable in a  term (0 if no variable is included)
maxVarInTerm:: Term -> VarIndex
maxVarInTerm (Var v)     = v
maxVarInTerm (Comb s []) = 0
maxVarInTerm (Comb s ts) 
    | termListHasVar ts  = maximum (map maxVarInTerm ts)
    | otherwise          = 0

