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
--den Fall gibt es gar nicht
-- dot (Comb c1 []) (Comb c2 [])      = "[" ++ c1 ++ "," ++ c2 ++ "]"
dot (Comb c []) (Comb "." (t1:t2)) = "[" ++ c ++ "," ++ (removeBrackets (dot t1 (head t2))) ++ "]"



-- macht Rules lesbar
instance Pretty Rule where
    pretty (rh :- []) = pretty rh ++ "."
    pretty (rh :- rt) = pretty rh ++ " :- " ++ (concat (intersperse "," (map pretty rt))) ++ "."

instance Pretty Goal where
    pretty (Goal ts) = "?- " ++ (concat (intersperse ", " (map pretty ts))) ++ "."

instance Pretty Prog where
    pretty (Prog rs) = concat (intersperse "\n\n" (map pretty rs))

instance Pretty SLDTree where
    pretty (SLDTree (Goal ts) []) = "ASDF"
