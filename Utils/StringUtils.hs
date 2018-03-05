module Utils.StringUtils where

-- wandelt einenen einzelnen char in eine einelementige liste von chars um (-> String)
charToString :: Char -> String
charToString c = [c]

charListToStringList :: [Char] -> [String]
charListToStringList []     = []
charListToStringList (x:xs) = charToString x:charListToStringList xs

-- wandelt eine Liste von Strings in einen einzelnen String um (& setzt Kommas)
listToString :: [String] -> String
listToString []     = []
listToString [x]    = x
listToString (x:xs) = x ++ "," ++ (listToString xs)

-- entfernt Ecke Klammern am anfang und Ende eines Strings
removeBrackets :: String -> String 
removeBrackets (_:xs) = reverse (removeHead (reverse xs))
    where
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

-- hält das Gesamte Alphabet von A-Z als liste von chars
alphabet :: String
alphabet = ['A'..'Z']

-- hät alle Zahlen von 1-9 in als List von Chars
numbers :: [Int]
numbers = [1..]

-- Variable pretty Index
prettyVarNames :: [String]
prettyVarNames = (charListToStringList alphabet) ++ (concat (map (helper alphabet) (map show numbers)))
    where 
    helper :: [Char] -> [Char] -> [String]
    helper letters number = map (helper' number) letters
        where
        helper' :: [Char] -> Char -> String
        helper' number letter = letter:number

seperator :: String
seperator = "\n" ++ (take 40 endlessMinus) ++ "\n"

endlessMinus :: String
endlessMinus = '-':endlessMinus