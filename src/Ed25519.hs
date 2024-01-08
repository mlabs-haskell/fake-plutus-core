{-# LANGUAGE NoImplicitPrelude #-}

-- Based on https://ed25519.cr.yp.to/python/ed25519.py
module Ed25519 (checkValid) where

import Bitwise (indexBit)
import Control.Category ((.))
import Core
  ( addInteger,
    appendBS,
    bsToInteger,
    eqInteger,
    integerToBS,
    ite,
    mulInteger,
    quotInteger,
    remInteger,
    sliceBS,
    subInteger,
  )
import Data.Bool (Bool (False))
import Data.ByteString (ByteString)
import Derived (and, eqBool, not, oddInteger)
import GHC.Integer (Integer)
import SHA512 (sha512)

-- No checks for size, though we should be doing them.
checkValid ::
  ByteString ->
  ByteString ->
  ByteString ->
  Bool
checkValid signature message pubKey =
  let r = decodePoint (sliceBS signature 0 32)
      a = decodePoint pubKey
      s = decodeInt (sliceBS signature 32 32)
      h = hint (appendBS (encodePoint r) (appendBS pubKey message))
   in eqPoint (scalarMult bPoint s) (edwards r (scalarMult a h))

-- Helpers

newtype Point = Point (Integer, Integer)

eqPoint :: Point -> Point -> Bool
eqPoint (Point (x, y)) (Point (x1, y1)) =
  and (eqInteger x x1) (eqInteger y y1)

-- 2 ^ 255 - 19, but as a constant
q :: Integer
q = 57896044618658097711785492504343953926634992332820282019728792003956564819949

bx :: Integer
bx = xRecover by

by :: Integer
by = mulInteger 4 (inv 5)

bPoint :: Point
bPoint = Point (remInteger bx q, remInteger by q)

-- No on-curve checks are done here
decodePoint :: ByteString -> Point
decodePoint bs =
  let y = bsToInteger False bs
      x = xRecover y
      -- Due to endianness and how we index, we have to phrase this a bit
      -- differently to the original.
      cond = eqBool (oddInteger x) (indexBit bs 7)
   in ite
        cond
        (Point (subInteger q x, y))
        (Point (x, y))

decodeInt :: ByteString -> Integer
decodeInt = bsToInteger False

hint :: ByteString -> Integer
hint = bsToInteger False . sha512

encodePoint :: Point -> ByteString
encodePoint (Point (_, y)) = integerToBS False 32 y

scalarMult :: Point -> Integer -> Point
scalarMult p e =
  ite
    (eqInteger e 0)
    (Point (0, 1))
    ( let q' = scalarMult p (quotInteger e 2)
          q'' = edwards q' q'
       in ite (oddInteger e) (edwards q'' p) q''
    )

d :: Integer
d = mulInteger (subInteger 1 121665) (inv 121666)

edwards :: Point -> Point -> Point
edwards (Point (x1, y1)) (Point (x2, y2)) =
  let pointSmush = mulInteger x1 (mulInteger x2 (mulInteger y1 y2))
      x3InvExp = addInteger 1 (mulInteger d pointSmush)
      y3InvExp = subInteger 1 (mulInteger d pointSmush)
      x3 =
        mulInteger
          (addInteger (mulInteger x1 y2) (mulInteger x2 y1))
          (inv x3InvExp)
      y3 =
        mulInteger
          (addInteger (mulInteger y1 y2) (mulInteger x1 x2))
          (inv y3InvExp)
   in Point (remInteger x3 q, remInteger y3 q)

inv :: Integer -> Integer
inv x = expMod x (subInteger q 2) q

expMod :: Integer -> Integer -> Integer -> Integer
expMod b' e m =
  ite
    (eqInteger e 0)
    1
    ( let reduced = expMod b' (quotInteger e 2) m
          t = remInteger (mulInteger reduced reduced) m
       in ite (oddInteger e) (remInteger (mulInteger t b') m) t
    )

i :: Integer
i = expMod 2 (quotInteger (subInteger i 1) 4) q

xRecover :: Integer -> Integer
xRecover y =
  let xx = mulInteger (mulInteger y (subInteger y 1)) (inv (mulInteger d (mulInteger y (addInteger y 1))))
      x = expMod xx (quotInteger (addInteger q 3) 8) q
      cond1 = not (eqInteger 0 (remInteger (subInteger (mulInteger x x) xx) q))
      cond2 = oddInteger x
      cond1Res = remInteger (mulInteger x i) q
      cond2Res = subInteger q x
      cond12Res = subInteger q cond1Res
   in ite cond1 (ite cond2 cond12Res cond1Res) (ite cond2 cond2Res x)
