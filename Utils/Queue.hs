module Utils.Queue where

import Type

-- DataType for a Que of (Subst,SLDTree); made for BFS Search

type QueueType a = [a]
type Queue = QueueType (Subst,SLDTree)

-- returns the last element of the queue list
getElement :: Queue -> Maybe (Queue,(Subst,SLDTree))
getElement q = case length q of
    0 -> Nothing
    otherwise ->  Just(init q,last q)

-- adds an Element to the Queue, to the end of the list
addElement :: Queue -> (Subst,SLDTree) -> Queue
addElement q t = t:q
