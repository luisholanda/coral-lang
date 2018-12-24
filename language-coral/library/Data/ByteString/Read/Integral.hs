module Data.ByteString.Read.Integral where

import           GHC.TypeLits

import qualified Data.ByteString.Read.Class    as C


integral_ :: (C.Radix b, Num n, C.Source s) => proxy b -> s -> (n, Int, s)
integral_ pn = loop 0 0
 where
  loop !i !d !s
    | C.null s = (i, d, s)
    | not (C.isDigit pn $ C.head s) = (i, d, s)
    | otherwise = loop (i * fromIntegral (natVal pn) + new) (d + 1) (C.tail s)
    where new = fromIntegral . C.unsafeToDigit pn $ C.head s
{-# INLINABLE integral_ #-}


integral' :: (C.Radix b, Num n, C.Source s) => proxy b -> s -> Maybe (n, s)
integral' pn s0 = case integral_ pn s0 of
  (_, 0, _) -> Nothing
  (n, _, s) -> Just (n, s)
{-# INLINABLE integral' #-}
