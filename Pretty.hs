module Pretty where 

import Type

class Pretty a where 
    pretty :: a -> String

instance Pretty Term where

    pretty (Var x)               = charToString (alphabet !! x)
    pretty (Comb s [])           = s
    pretty (Comb "." (t1:t2))    = dot t1 (head t2)
    pretty (Comb s terms)        = s ++ "(" ++ listToString(map pretty terms) ++ ")"

dot :: Term -> Term -> String
dot (Var x) (Comb "[]" [])         = "[" ++ (charToString (alphabet !! x)) ++ "]"
dot (Var x) (Var y)                = "[" ++ (charToString (alphabet !! x)) ++ "|" 
                                         ++ (charToString (alphabet !! y)) ++ "]"
dot (Comb c []) (Comb "[]" [])     = "[" ++ c ++ "]"
dot (Comb c1 []) (Comb c2 [])      = "[" ++ c1 ++ "," ++ c2 ++ "]"
dot (Comb c []) (Comb "." (t1:t2)) = "[" ++ c ++ "," ++ (removeBrackets (dot t1 (head t2))) ++ "]"

alphabet :: [Char]
alphabet = ['A'..'Z']

charToString :: Char -> String
charToString c = [c]

listToString :: [String] -> String
listToString [x]    = x
listToString (x:xs) = x ++ "," ++ (listToString xs)

removeBrackets :: String -> String 
removeBrackets (_:xs) = reverse (removeHead (reverse xs))

removeHead :: String -> String
removeHead (_:xs) = xs

isEmpty :: String -> Bool 
isEmpty s
    | s == "[]" = True
    | otherwise = False

isList :: String -> Bool
isList [x] = False
isList (x:xs) 
    | x == '[' = True
    | otherwise = False