module Utils.TermUtils where

import Type
import Pretty

-- True if given Term is Var
isVar :: Term -> Bool
isVar (Var i)    = True
isVar (Comb s t) = False

-- True wenn die eine Variable mit x im Term enthalten ist
checkVarInTerm :: Int -> Term -> Bool
checkVarInTerm x (Var y)
    | x == y    = True
    | otherwise = False
checkVarInTerm x (Comb _ ts) = elem True (map (checkVarInTerm x) ts)

-- Gibt True zurück wenn zwei Terme genau gleich sein
isTermEq :: Term -> Term -> Bool
isTermEq (Var x) (Var y) 
    | x == y                    = True
    | otherwise                 = False
isTermEq (Var x) (Comb s ts) = False
isTermEq (Comb s ts) (Var x) = False
isTermEq (Comb s1 t1s) (Comb s2 t2s)
    | s1 == s2                  = isTermListEq t1s t2s
    | otherwise                 = False

-- Gibt True zurück wenn alle Terme einer Liste genau gleich sind
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

-- True if Variable x ist im Term enthalten
termHasVar :: Term -> Bool
termHasVar (Var _)     = True
termHasVar (Comb _ []) = False  
termHasVar (Comb s t)  = termListHasVar t

-- True if Variable x ist im [Term] enthalten
termListHasVar :: [Term] -> Bool
termListHasVar []  = False
termListHasVar [t] = termHasVar t
termListHasVar (t:ts)
    | termHasVar t = True
    | otherwise    = termListHasVar ts

-- gibt eine Liste an Variablen zurück die in einem Term Vorkommen                             
getVarsInTerm :: Term -> [VarIndex]
getVarsInTerm (Var a)     = [a]
getVarsInTerm (Comb _ []) = []
getVarsInTerm (Comb _ ts) = getVarsInTermList ts

-- gibt die größte Variable in einer Liste von Termen zurück (0 wenn keine Variable enthalten ist)
maxVarInTermlist :: [Term] -> VarIndex
maxVarInTermlist []     = 0
maxVarInTermlist ts 
    | termListHasVar ts = maximum (map maxVarInTerm ts)
    | otherwise         = 0

-- gibt die größte Variable in einem Term zurück
maxVarInTerm:: Term -> VarIndex
maxVarInTerm (Var v)     = v
maxVarInTerm (Comb s []) = 0
maxVarInTerm (Comb s ts) 
    | termListHasVar ts  = maximum (map maxVarInTerm ts)
    | otherwise          = 0


    -- gibt alle Variablen zurück die in einer Liste von Termn vorkommen
getVarsInTermList :: [Term] -> [VarIndex]
getVarsInTermList []                 = []
getVarsInTermList ((Var x) : xs)     = [x] ++ (getVarsInTermList xs)
getVarsInTermList ((Comb _ xs) : ts) = (getVarsInTermList xs) ++ (getVarsInTermList ts)