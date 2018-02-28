module Substitution where

import Type

-- Erstellt eine leere Substitution
empty :: Subst
empty = Subst []

-- Erstellt eine Substitution die eine einzelne Variable auf einen Term abbildet
single :: VarIndex -> Term -> Subst
single v t = Subst [(Replace v t)]

-- wendet Substitution auf einen Term an
apply :: Subst -> Term -> Term
apply _ _ = (Var 1)

-- komponiert zwei Substitutionen
-- Remember: apply(compse s2 s1) t == apply s2 (apply s1 t)
compose :: Subst -> Subst -> Subst
compose _ _ = Subst []