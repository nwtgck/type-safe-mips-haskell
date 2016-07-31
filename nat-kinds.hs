{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.TypeLits

data Bit = O | I | X deriving Show

data Bits :: Nat -> * where
  End  :: Bits 0
  (:-) :: Bit -> Bits n -> Bits (n+1)

inflixr 0 :-

headBits :: Bits (n+1) -> Bit
headBits (b:-bs) = b

main :: IO ()
main = do
  let bits = O:-I:-O:-I:-End
  print "hello"
