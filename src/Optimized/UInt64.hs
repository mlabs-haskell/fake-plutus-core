{-# LANGUAGE NoImplicitPrelude #-}

module Optimized.UInt64
  ( UInt64,
    toUInt64,
    fromUInt64,
    shiftUInt64,
    rotateUInt64,
    andUInt64,
  )
where

import Bitwise (andBS, rotateBS, shiftBS)
import Core (bsToInteger, integerToBS)
import Data.Bool (Bool (True))
import Data.ByteString (ByteString)
import Data.Function (($))
import GHC.Num (Integer)

newtype UInt64 = UInt64 ByteString

toUInt64 :: Integer -> UInt64
toUInt64 i = UInt64 $ integerToBS True 8 i

fromUInt64 :: UInt64 -> Integer
fromUInt64 (UInt64 bs) = bsToInteger True bs

-- Copies Data.Bits semantics (positive means left, negative means right)
shiftUInt64 :: UInt64 -> Integer -> UInt64
shiftUInt64 (UInt64 bs) shift = UInt64 $ shiftBS shift bs

-- Copies Data.Bits semantics (positive means left, negative means right)
rotateUInt64 :: UInt64 -> Integer -> UInt64
rotateUInt64 (UInt64 bs) rotation = UInt64 $ rotateBS rotation bs

andUInt64 :: UInt64 -> UInt64 -> UInt64
andUInt64 (UInt64 x) (UInt64 y) = UInt64 $ andBS 8 x y
