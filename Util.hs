module Util(
  listUpdate,
  goOnRight
) where

import           FRP.Yampa

-- Update a List by specific index
listUpdate :: [a] -> Int -> a -> [a]
listUpdate []     _   _ = error "Can't update list"
listUpdate (x:xs) 0   v = v : xs
listUpdate (x:xs) idx v = x: listUpdate xs (idx-1) v


-- If left is Event a, go on to Right Event
goOnRight :: Event a -> Event b -> Event b
goOnRight NoEvent _ = NoEvent
goOnRight _       r = r
