module Sub where

import Type

-- Erstellt eine leere Substitution
empty :: Subst
empty = Subst []

-- Erstellt eine Substitution die eine einzelne Variable 
-- auf einen Term abbildet
single :: VarIndex -> Term -> Subst
single var term = Subst [(Replace var term)]

-- wendet Substitution auf einen Term an
apply :: Subst -> Term -> Term

-- Subst leer
apply (Subst []) t                            = t

-- Term ist konstante
apply _ (Comb c [])                           = (Comb c [])

-- Term ist eine Variable
-- apply (Subst [(Replace i t)]) (Var v)         = if v == i then t else (Var v)
apply (Subst ((Replace i t):rs)) (Var v)      = if v == i then t else apply (Subst rs) (Var v)

-- Term ist 'noch' komplizierter
apply sub (Comb s terms)                      = (Comb s (map (apply sub) terms))


-- komponiert zwei Substitutionen
-- Remember: apply(compse s2 s1) t == apply s2 (apply s1 t)
compose :: Subst -> Subst -> Subst
compose _ _ = Subst []