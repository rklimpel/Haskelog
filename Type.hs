module Type
  ( VarIndex, Strategy, 
    Term(..), Rule(..), Prog(..), Goal(..), Subst(..), Replace(..), SLDTree(..)
  ) where

-- Alias type for variables
type VarIndex = Int

-- Alias type for Search Strategys
type Strategy = SLDTree -> [Subst]

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]
  deriving (Show,Eq)

-- Data type for varibale replacements in substitutions
--(Replace VarIndex -> Term)
data Replace = Replace VarIndex Term
  deriving (Show,Eq)

-- Data type for subsitutions
-- (as List of single Replacements)
data Subst = Subst [Replace]
  deriving (Show,Eq)

-- Data type for SLDTrees
data SLDTree = SLDTree Goal [(Subst,SLDTree)]
  deriving Show

-- Data type for program rules
data Rule = Term :- [Term]
  deriving Show

-- Data type for programs
data Prog = Prog [Rule]
  deriving Show

-- Data type for goals
data Goal = Goal [Term]
  deriving Show
