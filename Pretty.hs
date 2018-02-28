module Pretty where 

import Type

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
alphabet :: [Char]
alphabet = ['A'..'Z']

-- wandelt einenen einzelnen char in eine einelementige liste von chars um (-> String)
charToString :: Char -> String
charToString c = [c]

-- wandelt eine Liste von Strings in einen einzelnen String um (& setzt Kommas)
listToString :: [String] -> String
listToString [x]    = x
listToString (x:xs) = x ++ "," ++ (listToString xs)

-- entfernt Ecke Klammern am anfang und Ende eines Strings
removeBrackets :: String -> String 
removeBrackets (_:xs) = reverse (removeHead (reverse xs))

-- entfernt das erste Element eines Strings
removeHead :: String -> String
removeHead (_:xs) = xs

-- gibt True zurück wenn ein String folgende gestalt hat: "[]"
isEmpty :: String -> Bool 
isEmpty s
    | s == "[]" = True
    | otherwise = False

-- gibt True zrück wenn ein String mit "[" beginnt 
isList :: String -> Bool
isList [x] = False
isList (x:xs) 
    | x == '[' = True
    | otherwise = False


