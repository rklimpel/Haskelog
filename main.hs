module Main (main) where

import qualified Interactive
import Test.Test


{-

Welcome to Haskelog - Simple Prolog Interpreter written in Haskell.

- Type main to Enter the Interactive Prolog Shell
- Lookup Functions in /Test/Test.hs and use them to Test the Projekt with our samples

Rico & Gianmarco @2018

TODO:

- Anonyme Variablen nicht anzeigen
- Variablennamen überschneidungen (Nutzereingabe aus prettyVarNames löschen)

-}

main :: IO()
main = Interactive.main
