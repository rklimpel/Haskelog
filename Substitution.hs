module Substitution where

import Type

-- Erstellt eine leere Substitution
empty :: Subst

-- Erstellt eine Substitution die eine einzelne Variable auf einen Term abbildet
single :: VarIndex -> Term -> Subst

-- wendet Substitution auf einen Term an
apply :: Subst -> Term -> Term

-- komponiert zwei Substitutionen
compose :: Subst -> Subst -> Subst