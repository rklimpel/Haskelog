module SLDTree where

import Type

-- konstruiert den SLD-Baum zu einem Programm und einer Anfrage
-- Selektionsstrategie FIRST (es wird immer das linkeste Literal zum Beweisen ausgewählt)
sld :: Prog -> Goal -> SLDTree
sld _ _ = SLDTree (Goal []) []