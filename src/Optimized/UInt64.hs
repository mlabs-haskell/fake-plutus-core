{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Optimized.UInt64
  ( UInt64,
    toUInt64,
    fromUInt64,
    shiftUInt64,
    rotateUInt64,
    andUInt64,
    addOverflowUInt64,
    addUInt64,
  )
where

import Bitwise (andBS, rotateBS, shiftBS)
import Control.DeepSeq (NFData)
import Core
  ( addInteger,
    bsToInteger,
    eqInteger,
    integerToBS,
    ite,
    lenBS,
    remInteger,
    sliceBS,
  )
import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString)
import Data.Function (($))
import GHC.Num (Integer)

newtype UInt64 = UInt64 ByteString
  deriving (NFData) via ByteString

toUInt64 :: Integer -> UInt64
toUInt64 i = UInt64 $ integerToBS True 8 i

fromUInt64 :: UInt64 -> Integer
fromUInt64 (UInt64 bs) = bsToInteger True bs

addUInt64 :: UInt64 -> UInt64 -> UInt64
addUInt64 (UInt64 x) (UInt64 y) =
  let added = addInteger (bsToInteger True x) (bsToInteger True y)
      reduced = remInteger added limit64
   in toUInt64 reduced

-- Copies Data.Bits semantics (positive means left, negative means right)
shiftUInt64 :: UInt64 -> Integer -> UInt64
shiftUInt64 (UInt64 bs) shift = UInt64 $ shiftBS shift bs

-- Copies Data.Bits semantics (positive means left, negative means right)
rotateUInt64 :: UInt64 -> Integer -> UInt64
rotateUInt64 (UInt64 bs) rotation = UInt64 $ rotateBS rotation bs

andUInt64 :: UInt64 -> UInt64 -> UInt64
andUInt64 (UInt64 x) (UInt64 y) = UInt64 $ andBS 8 x y

addOverflowUInt64 :: UInt64 -> UInt64 -> (Bool, UInt64)
addOverflowUInt64 (UInt64 x) (UInt64 y) =
  let added = addInteger (bsToInteger True x) (bsToInteger True y)
      converted = integerToBS True 0 added
      len = lenBS converted
   in ite
        (eqInteger len 8)
        (False, UInt64 converted)
        (True, UInt64 (sliceBS converted 1 8))

-- Helpers

limit64 :: Integer
limit64 = 18_446_744_073_709_551_616
