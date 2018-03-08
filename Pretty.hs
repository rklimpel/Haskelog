{-# LANGUAGE TypeSynonymInstances #-}

module Pretty where

import Type

import Utils.StringUtils

import Data.List

class Pretty a where
    pretty :: a -> String
    prettyWithVars :: [(VarIndex, String)] -> a -> String

-- converts a variable to Prolog representation 
instance Pretty VarIndex where
    pretty i = prettyWithVars [] i
    prettyWithVars realNames i = case lookup i realNames of
                                    -- take real Var Name from list if there is one
                                    Just n  -> n
                                    -- take the PrettyVarName which stands at the position of the number of the variable
                                    Nothing -> prettyVarNames !! i -- TODO!

-- converts a term to Prolog representation 
instance Pretty Term where
    pretty t = prettyWithVars [] t
    prettyWithVars realNames (Var x)                          = prettyWithVars realNames x
    prettyWithVars realNames (Comb "." [head,Comb "[]" []])   = "[" ++ (prettyWithVars realNames head) ++ "]"
    prettyWithVars realNames (Comb "." [head,Comb "." terms]) = "[" ++ (prettyWithVars realNames head) ++ "," 
                                                                    ++ (removeBrackets (prettyWithVars realNames (Comb "." terms))) ++ "]"
    prettyWithVars realNames (Comb "." [head,tail])           = "[" ++ (prettyWithVars realNames head) ++ "|"
                                                                    ++ (prettyWithVars realNames tail) ++ "]"
    prettyWithVars realNames (Comb s [])                      = s
    prettyWithVars realNames (Comb s terms)                   = s ++ "(" ++ listToString(map (prettyWithVars realNames) terms) ++ ")"

-- converts a rule to Prolog representation 
instance Pretty Rule where
    pretty r = prettyWithVars [] r
    prettyWithVars realNames (rh :- []) = (prettyWithVars realNames rh) ++ "."
    prettyWithVars realNames (rh :- rt) = (prettyWithVars realNames rh) ++ " :- " 
                                          ++ (concat (intersperse "," (map (prettyWithVars realNames) rt))) ++ "."

-- converts a goal to Prolog representation 
instance Pretty Goal where
    pretty g = prettyWithVars [] g
    prettyWithVars realNames (Goal ts) = "?- " ++ (concat (intersperse ", " (map (prettyWithVars realNames) ts))) ++ "."

-- converts a program to Prolog representation 
instance Pretty Prog where
    pretty p = prettyWithVars [] p
    prettyWithVars realNames (Prog rs) = concat (intersperse "\n" (map (prettyWithVars realNames) rs))

-- converts a substitution to Prolog representation 
instance Pretty Subst where
    pretty s = prettyWithVars [] s
    prettyWithVars realNames (Subst r) = "= {" ++ (listToString (map (pretReplace realNames) r)) ++ "}"
        where
        -- converts a single replace to Prolog representation 
        pretReplace :: [(VarIndex,String)] -> Replace -> String
        pretReplace realNames (Replace i t) = (prettyWithVars realNames i) ++ " -> " ++ (prettyWithVars realNames t)

-- converts a SLDTree to Prolog representation 
instance Pretty SLDTree where
    pretty t = prettyWithVars [] t
    prettyWithVars realNames (SLDTree (Goal ts) ledges) = (prettyWithVars realNames (Goal ts)) ++ "\n" ++ (concatMap (prettyLedges 0) ledges)
        where 
            prettyLedges k ((Subst rs),(SLDTree (Goal ts) ledges)) =   
                                vertLines k ++ "+--" ++ (prettyWithVars realNames (Subst rs)) ++ "\n"
                                ++ vertLines (k+1) ++ (prettyWithVars realNames (Goal ts)) ++ "\n"
                                ++ concatMap (prettyLedges (k+1)) ledges
                where
                    vertLines k = concat (take k (repeat "|   "))