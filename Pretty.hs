module Pretty where 

import Type
import Utils.StringUtils

class Pretty a where 
    pretty :: a -> String

-- macht Substitutionen lesbar 
instance Pretty Subst where
    pretty (Subst r) = "{" ++ (listToString (map pretReplace r)) ++ "}"

-- kümmert sich um einzelne Replace Statements
pretReplace :: Replace -> String
pretReplace (Replace i t) = (pretty (Var i)) ++ " -> " ++ (pretty t)

-- macht Terme lesbar (gültige Prolog darstellung)
instance Pretty Term where
    pretty (Var x)               = charToString (alphabet !! x)
    pretty (Comb s [])           = s
    pretty (Comb "." (t1:t2))    = dot t1 (head t2)
    pretty (Comb s terms)        = s ++ "(" ++ listToString(map pretty terms) ++ ")"

-- Verarbeitet die "." Operation    
dot :: Term -> Term -> String
dot (Var x) (Comb "[]" [])         = "[" ++ (charToString (alphabet !! x)) ++ "]"
dot (Var x) (Var y)                = "[" ++ (charToString (alphabet !! x)) ++ "|" 
                                         ++ (charToString (alphabet !! y)) ++ "]"
dot (Comb c []) (Comb "[]" [])     = "[" ++ c ++ "]"
dot (Comb c1 []) (Comb c2 [])      = "[" ++ c1 ++ "," ++ c2 ++ "]"
dot (Comb c []) (Comb "." (t1:t2)) = "[" ++ c ++ "," ++ (removeBrackets (dot t1 (head t2))) ++ "]"

instance Pretty Rule where
    pretty r = (show r)

instance Pretty Goal where
    pretty g = (show g)

instance Pretty Prog where
    pretty p = (show p)

instance Pretty SLDTree where
    pretty sld = (show sld)