{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Concurrent
import           Control.Monad
import           Data.Bits          (shift, (.|.))
import           Data.IORef
import           Data.Maybe
import           FRP.Yampa
import           Unsafe.Coerce


-- data N0
-- data Succ n
--
-- class Nat n where
--   num :: n -> Int
--
-- instance Nat N0 where
--   num _ = 0
--
-- instance Nat n => Nat (Succ n) where
--   num _ = 1 + (num (undefined :: n))

-- type N1 = Succ N0
-- type N2 = Succ N1
-- type N3 = Succ N2
-- type N4 = Succ N3
-- type N5 = Succ N4

-- type family Add :: Nat -> Nat -> * where
--   Add n 0          = n                     -- a add 0 = a
--   Add 0 n           = n                     -- 0 add b = b
--   Add (Succ a) b = Succ(Add a b) -- (a +1) add (b +1) = (a add b) +1 +1
--
-- type family (Nat s, Nat n) => Range s n :: * where
--   Range n N0 = Bits n
--   Range N0 n = Bits n
--   Range (Succ a) b = Bits (Succ (Add a b))
--
-- -- O, I, X mean 0, 1, x respectively
-- data Bit = O | I | X deriving Show
--
-- data Bits n = Bits [Bit] deriving Show -- Bits synonim can be changed, so I use the synonim


main :: IO ()
main = return ()
