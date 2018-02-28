module Sub where

import Type

-- Erstellt eine leere Substitution
empty :: Subst
empty = Subst []

-- Erstellt eine Substitution die eine einzelne Variable auf einen Term abbildet
single :: VarIndex -> Term -> Subst
single var term = Subst [(Replace var term)]

-- wendet Substitution auf einen Term an
apply :: Subst -> Term -> Term
apply (Subst [r]) (Comb c [])= (Comb c [])
apply (Subst (r:rs)) (Comb c []) = (Comb c [])
apply (Subst [r]) t = applySingle r t
apply (Subst (r:rs)) t = apply (Subst rs) (applySingle r t)

applySingle :: Replace -> Term -> Term
applySingle (Replace i r) t = (Var 0)

-- komponiert zwei Substitutionen
-- Remember: apply(compse s2 s1) t == apply s2 (apply s1 t)
compose :: Subst -> Subst -> Subst
compose _ _ = Subst []