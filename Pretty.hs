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


-- dot (Var x) (Comb c2 [])       ="[" ++  (charToString (alphabet !! x)) ++ "," ++ c2 ++ "]"

--sonderfall hier kann man die brankets nicht entfernen das stimmt sonst nicht logisch
--kann aber defininitiv besser gemacht werden siehe term9

--was gemacht werden soll das
dot (Var x) (Comb "." [Comb c [], Comb "[]" []] )         = "[" ++  (charToString (alphabet !! x)) ++ ","  ++ c ++ "]"

dot (Var x) (Comb "." (t1:t2))     = "[" ++  (charToString (alphabet !! x)) ++ "|" ++ (dot t1 (head t2)) ++ "]"

dot (Comb c []) (Comb "[]" [])     = "[" ++ c ++ "]"

<<<<<<< HEAD
--den Fall gibt es gar nicht
-- dot (Comb c1 []) (Comb c2 [])      = "[" ++ c1 ++ "," ++ c2 ++ "]"
dot (Comb c []) (Comb "." (t1:t2)) = "[" ++ c ++ "," ++ (removeBrackets (dot t1 (head t2))) ++ "]"
=======
instance Pretty Rule where
    pretty r = (show r)

instance Pretty Goal where
    pretty g = (show g)

instance Pretty Prog where
    pretty p = (show p)

instance Pretty SLDTree where
    pretty sld = (show sld)
>>>>>>> 0831784caaedef6c01245172fbecdd2fc13ac00b
