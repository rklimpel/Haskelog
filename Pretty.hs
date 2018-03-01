module Pretty where 

import Type
import Utils.StringUtils

class Pretty a where 
    pretty :: a -> String

-- macht Substitutionen lesbar 
instance Pretty Subst where
    pretty (Subst r) = "{" ++ (listToString (map pretReplace r)) ++ "}"

pretReplace :: Replace -> String
pretReplace (Replace i t) = (pretty (Var i)) ++ " -> " ++ (pretty t)

-- Macht Terme lesbar (gültige Prolog darstellung)
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

-- hält das Gesamte Alphabet von A-Z als liste von chars
alphabet :: String
alphabet = ['A'..'Z']



