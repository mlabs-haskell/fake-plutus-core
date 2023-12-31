{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Baseline.UInt64
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

import Control.DeepSeq (NFData)
import Core
  ( addInteger,
    eqInteger,
    ite,
    leInteger,
    ltInteger,
    mulInteger,
    quotInteger,
    remInteger,
    subInteger,
  )
import Data.Bool (Bool (False, True))
import Data.Function (($))
import Derived
  ( absInteger,
    minInteger,
    powInteger,
  )
import GHC.Err (error)
import GHC.Num (Integer)

newtype UInt64 = UInt64 Integer
  deriving (NFData) via Integer

addUInt64 :: UInt64 -> UInt64 -> UInt64
addUInt64 (UInt64 x) (UInt64 y) =
  let added = addInteger x y
   in UInt64 $ remInteger added limit64

toUInt64 :: Integer -> UInt64
toUInt64 i =
  ite
    (ltInteger i 0)
    (error "can't convert a negative number")
    (UInt64 $ remInteger i limit64)

fromUInt64 :: UInt64 -> Integer
fromUInt64 (UInt64 i) = i

addOverflowUInt64 :: UInt64 -> UInt64 -> (Bool, UInt64)
addOverflowUInt64 (UInt64 x) (UInt64 y) =
  let added = addInteger x y
      overflow = quotInteger added limit64
   in ite
        (ltInteger 0 overflow)
        (True, UInt64 $ remInteger added limit64)
        (False, UInt64 added)

-- Copies Data.Bits semantics (positive means left, negative means right)
shiftUInt64 :: UInt64 -> Integer -> UInt64
shiftUInt64 uint@(UInt64 i) shift =
  ite
    (eqInteger shift 0)
    uint
    ( let absShift = absInteger shift
       in UInt64
            ( ite
                (leInteger 64 absShift)
                0
                ( ite
                    (ltInteger shift 0)
                    (shiftRight absShift)
                    (shiftLeft absShift)
                )
            )
    )
  where
    shiftRight :: Integer -> Integer
    shiftRight actualShift = quotInteger i $ powInteger 2 actualShift
    shiftLeft :: Integer -> Integer
    shiftLeft actualShift =
      remInteger (mulInteger i $ powInteger 2 actualShift) limit64

-- Copies Data.Bits semantics (positive means left, negative means right)
rotateUInt64 :: UInt64 -> Integer -> UInt64
rotateUInt64 uint@(UInt64 i) rotation =
  ite
    (eqInteger rotation 0)
    uint
    ( let absRotation = absInteger rotation
          reducedRotation = remInteger absRotation 64
       in UInt64 $
            ite
              (ltInteger rotation 0)
              (rotateRight reducedRotation)
              (rotateLeft reducedRotation)
    )
  where
    rotateRight :: Integer -> Integer
    rotateRight actualRotation =
      let raised = powInteger 2 actualRotation
          rightBit = quotInteger i raised
          leftBit = remInteger i raised
       in addInteger rightBit $ mulInteger leftBit raised
    rotateLeft :: Integer -> Integer
    rotateLeft actualRotation =
      let raised = powInteger 2 actualRotation
          q = quotInteger i raised
          r = remInteger i raised
       in addInteger q $ mulInteger r raised

andUInt64 :: UInt64 -> UInt64 -> UInt64
andUInt64 (UInt64 x) (UInt64 y) = UInt64 $ go 0 x y 1 64
  where
    go ::
      Integer ->
      Integer ->
      Integer ->
      Integer ->
      Integer ->
      Integer
    go acc currX currY scale count =
      ite
        (eqInteger count 0)
        acc
        ( let newX = quotInteger currX 2
              bitX = remInteger currX 2
              newY = quotInteger currY 2
              bitY = remInteger currY 2
              bit = minInteger bitX bitY
              shifted = mulInteger bit scale
           in go
                (addInteger acc shifted)
                newX
                newY
                (mulInteger scale 2)
                (subInteger count 1)
        )

-- Helpers

limit64 :: Integer
limit64 = 18_446_744_073_709_551_616
