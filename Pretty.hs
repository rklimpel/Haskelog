module Pretty where 

import Type

class Pretty a where 
    pretty :: a -> String


data VarTerm = VarTerm Char Term

instance Pretty Term where

    pretty (Var x)               = "Var " ++ (show x)
    pretty (Comb s [])           = s
    pretty (Comb "." terms)      = dot terms
    pretty (Comb s terms)        = s ++ "(" ++ listToString(map pretty terms) ++ ")"
    pretty (Comb s [t])          = pretty t
    pretty (Comb s (t:ts))       = listToString (map pretty (t:ts))



listToString :: [String] -> String
listToString [x]    = x
listToString (x:xs) = x ++ "," ++ (listToString xs)

dot :: [Term] -> String
dot [t] = pretty t
dot (t:ts) = let a = (pretty t)
                 b = (pretty (head ts))
             in if (isEmpty b) then "[" ++ a ++ "]"
                else if (isList b) then  "[" ++ a ++ "," ++ (removeBrackets b) ++ "]"
                else "[" ++ a ++ "," ++ b ++ "]"

isEmpty :: String -> Bool 
isEmpty s
    | s == "[]" = True
    | otherwise = False

removeBrackets :: String -> String 
removeBrackets (_:xs) = reverse (removeHead (reverse xs))

removeHead :: String -> String
removeHead (_:xs) = xs

isList :: String -> Bool
isList [x] = False
isList (x:xs) 
    | x == '[' = True
    | otherwise = False