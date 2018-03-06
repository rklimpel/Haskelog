module Utils.SubUtils where

import Type

-- get VarIndex from Replacement
getIndex :: Replace -> VarIndex
getIndex (Replace i t) = i

-- get Term from Replacement
getTerm ::  Replace -> Term
getTerm (Replace i t) = t

-- Build [Replace] List form [VarIndex] and [Term] Lists
buildReplace :: [VarIndex] -> [Term] -> [Replace]
buildReplace is ts = buildReplaceH is ts []
    where
    buildReplaceH :: [VarIndex] -> [Term] -> [Replace] -> [Replace]
    buildReplaceH [i] [t] r         = r ++ [(Replace i t)]
    buildReplaceH (i:is) (t:ts) r   =  [(Replace i t)] ++ (buildReplaceH is ts r)