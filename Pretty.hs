module Pretty where 

import Type

class Pretty a where 
    pretty :: a -> String

instance Pretty Term where
    pretty (Comb "append" terms) = "append(" ++ (pretty terms) ++ ")"
    pretty (Comb "." terms)      = "." ++ (pretty terms)
    pretty (Comb s terms)        = s ++ (pretty terms)
    pretty (Var x)               = "var " ++ (show x)   

instance Pretty [a] where
    pretty [a] = ".A"
    pretty (a:as) = ".AS"