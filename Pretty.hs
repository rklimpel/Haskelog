module Pretty where

import Type
import Utils.StringUtils
import Data.List

class Pretty a where
    pretty :: a -> String

-- macht Substitutionen lesbar
instance Pretty Subst where
    pretty (Subst r) = "sigma = {" ++ (listToString (map pretReplace r)) ++ "}"

-- kümmert sich um einzelne Replace Statements
pretReplace :: Replace -> String
pretReplace (Replace i t) = (pretty (Var i)) ++ " -> " ++ (pretty t)

-- macht Terme lesbar (gültige Prolog darstellung)
instance Pretty Term where
    pretty (Var x)               = prettyVarNames !! x
    pretty (Comb s [])           = s
    pretty (Comb "." (t1:t2))    = dot t1 (head t2)
    pretty (Comb s terms)        = s ++ "(" ++ listToString(map pretty terms) ++ ")"

-- Verarbeitet die "." Operation
dot :: Term -> Term -> String
dot (Var x) (Comb "[]" [])         = "[" ++ (charToString (alphabet !! x)) ++ "]"
dot (Var x) (Var y)                = "[" ++ (charToString (alphabet !! x)) ++ "|"
                                         ++ (charToString (alphabet !! y)) ++ "]"

-- TODO Gianmarco Comments...                                         
dot (Var x) (Comb "." [Comb c [], Comb "[]" []] ) = "[" ++  (pretty (Var x)) ++ ","  ++ c ++ "]"
dot (Var x) (Comb "." (t1:t2))                    = "[" ++  (pretty (Var x)) ++ "|" ++ (dot t1 (head t2)) ++ "]"
-- konstante & variable
dot (Comb c []) (Var x)                           = "[" ++ c ++ "|" ++ (pretty (Var x)) ++ "]" 
-- konstante & leere list -> konstante wird alleine in Liste geschrieben 
dot (Comb c []) (Comb "[]" [])                    = "[" ++ c ++ "]"
dot (Comb c []) (Comb "." (t1:t2))                = "[" ++ c ++ "," ++ (removeBrackets (dot t1 (head t2))) ++ "]"



-- macht Rules lesbar
instance Pretty Rule where
    pretty (rh :- []) = pretty rh ++ "."
    pretty (rh :- rt) = pretty rh ++ " :- " ++ (concat (intersperse "," (map pretty rt))) ++ "."

instance Pretty Goal where
    pretty (Goal ts) = "?- " ++ (concat (intersperse ", " (map pretty ts))) ++ "."

instance Pretty Prog where
    pretty (Prog rs) = concat (intersperse "\n" (map pretty rs))

instance Pretty SLDTree where
    pretty (SLDTree (Goal ts) ledges) = (pretty (Goal ts)) ++ "\n" ++ (concatMap (prettyLedges 0) ledges)
        where 
            prettyLedges k ((Subst rs),(SLDTree (Goal ts) ledges)) =   
                                vertLines k ++ "+--" ++ pretty (Subst rs) ++ "\n"
                                ++ vertLines (k+1) ++ pretty (Goal ts) ++ "\n"
                                ++ concatMap (prettyLedges (k+1)) ledges
                where
                    vertLines k = concat (take k (repeat "|   "))

data Solution = Solution [Subst]

instance Pretty Solution where
    pretty (Solution subst) = concat ( intersperse "\n" (map pretty subst)) ++ "\n"