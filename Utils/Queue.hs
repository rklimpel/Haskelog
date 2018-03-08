module Utils.Queue where

import Type

type QueueType a = [a]
type Queue = QueueType (Subst,SLDTree)

-- returns the last element of the Queue and the new Queue without the last Element
getElement :: Queue -> Maybe (Queue,(Subst,SLDTree))
getElement q = case length q of
    0 -> Nothing
    otherwise ->  Just(init q,last q)

-- adds an Element to the Queue (to the top of the list)
addElement :: Queue -> (Subst,SLDTree) -> Queue
addElement q t = t:q
