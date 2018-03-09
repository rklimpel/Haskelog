module Utils.StringUtils where

import Data.List

-- converts a single char into a single-element list of chars (-> String)
charToString :: Char -> String
charToString c = [c]

-- converts a list of chars into a list of strings
charListToStringList :: [Char] -> [String]
charListToStringList []     = []
charListToStringList (x:xs) = charToString x:charListToStringList xs

-- converts a list of strings into a single string (and inserts commas in between)
listToString :: [String] -> String
listToString []     = []
listToString [x]    = x
listToString xs     = concat (intersperse "," xs)

-- removes the first and last element of a string (used to remove the square chambers)
removeBrackets :: String -> String 
removeBrackets xs = init (tail xs)

-- returns true if a string has the form: "[]"
isEmpty :: String -> Bool 
isEmpty s
    | s == "[]" = True
    | otherwise = False

-- returns true if a string starts with "["
isList :: String -> Bool
isList []  = False
isList [x] = False
isList xs
    | head xs == '[' && last xs == ']' = True
    | otherwise = False
 
-- the entire alphabet of A-Z as a list of chars
alphabet :: String
alphabet = ['A'..'Z']

-- all numbers from 1-9 in as a list of Chars
numbers :: [Int]
numbers = [1..]

-- a list of variable names given to the variables from 0 - ?, infinite
-- A-Z + A1-B1 + A? - Z?
prettyVarNames :: [String]
prettyVarNames = (charListToStringList alphabet) 
                ++ (concat (map (helper alphabet) (map show numbers)))
    where 
    -- takes the whole alphabet and write number behind every letter
    helper :: [Char] -> String -> [String]
    helper letters number = map (helper' number) letters
        -- write a number behind a letter
        where
        helper' :: [Char] -> Char -> String
        helper' number letter = letter:number

-- builds a 40-character separator from the infinite list of '-'
seperator :: String
seperator = "\n" ++ (take 40 endlessMinus) ++ "\n"

-- infinite list of '-'
endlessMinus :: String
endlessMinus = '-':endlessMinus