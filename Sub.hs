module Sub (empty,single,apply,compose) where

import Type

import Utils.SubUtils

-- PUBLIC FUNTCTIONS

-- creates an empty substitution
empty :: Subst
empty = Subst []

-- creates a substitution that maps a single variable to a term
single :: VarIndex -> Term -> Subst
single var term = Subst [(Replace var term)]

-- applies substitution to a term
apply :: Subst -> Term -> Term
-- substitution empty -> term stays as itself
apply (Subst []) t                            = t
-- Term ist konstante
-- apply _ (Comb c [])                           = (Comb c []) -- durch anderen Fall schon abgedeckt
-- Term ist eine Variable
apply (Subst ((Replace i t):rs)) (Var v)      = if v == i then t else apply (Subst rs) (Var v)
-- Term ist 'noch' komplizierter
apply sub (Comb s terms)                      = (Comb s (map (apply sub) terms))

-- komponiert zwei Substitutionen
-- Remember: apply(compse s2 s1) t == apply s2 (apply s1 t)
compose :: Subst -> Subst -> Subst
-- Eine der Substitutionen ist leer
compose s1 (Subst [])           = s1
compose (Subst []) s2           = s2
-- Wende Subst1 auf Terme von Subst2 an
compose (Subst r1s) (Subst r2s) = Subst ((buildReplace (map getIndex r2s) (map (apply (Subst r1s)) (map getTerm r2s))) ++ r1s)
