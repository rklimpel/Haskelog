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


-- get VarIndex from Replacement
getIndex :: Replace -> VarIndex
getIndex (Replace i t) = i

-- get Term from Replacement
getTerm ::  Replace -> Term
getTerm (Replace i t) = t

-- Build [Replace] form [VarIndex] and [Term]
buildReplace :: [VarIndex] -> [Term] -> [Replace]
buildReplace is ts = buildReplaceH is ts []

-- Helper for buildReplace
buildReplaceH :: [VarIndex] -> [Term] -> [Replace] -> [Replace]
buildReplaceH [i] [t] r         = r ++ [(Replace i t)]
buildReplaceH (i:is) (t:ts) r   =  [(Replace i t)] ++ (buildReplaceH is ts r)