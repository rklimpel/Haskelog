module Utils.StringUtils where

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

-- hält das Gesamte Alphabet von A-Z als liste von chars
alphabet :: String
alphabet = ['A'..'Z']

seperator :: String
seperator = "\n" ++ (take 40 endlessMinus) ++ "\n"

endlessMinus :: String
endlessMinus = '-':endlessMinus