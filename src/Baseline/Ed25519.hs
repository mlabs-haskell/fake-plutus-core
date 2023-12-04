{-# LANGUAGE NoImplicitPrelude #-}

module Baseline.Ed25519 (verify) where

import Baseline.SHA512 (sha512)
import Core
  ( appendBS,
    bsToInteger,
    eqInteger,
    ite,
    lenBS,
    ltInteger,
    mulInteger,
    quotInteger,
    remInteger,
    sliceBS,
    subInteger,
  )
import Data.Bool (Bool (False, True), otherwise)
import Data.ByteString (ByteString)
import GHC.Err (error)
import GHC.Num (Integer)

-- Based on https://ed25519.cr.yp.to/python/ed25519.py
verify :: ByteString -> ByteString -> ByteString -> Bool
verify msg pk sig
  | not (eqInteger expectedSigLength (lenBS sig)) =
      error "signature length is wrong"
  | not (eqInteger expectedPKLength (lenBS pk)) =
      error "pubkey length is wrong"
  | otherwise =
      let r = decodePoint (sliceBS sig 0 rSpan)
          a = decodePoint pk
          s = bsToInteger False (sliceBS sig rSpan rSpan)
          h = hInt (appendBS (encodePoint r) (appendBS pk msg))
       in eqInteger (scalarMult bPoint s) (edwards r (scalarMult a h))
  where
    b :: Integer
    b = 256
    expectedSigLength :: Integer
    expectedSigLength = 64
    expectedPKLength :: Integer
    expectedPKLength = 32
    rSpan :: Integer
    rSpan = 32
    bPoint :: (Integer, Integer)
    bPoint = (remInteger bX q, remInteger bY q)
    -- Theoretically this is a constant, and can be precomputed, but I don't
    -- think anyone would actually try and do this.
    q :: Integer
    q = subInteger (powInteger 2 255) 19
    bY :: Integer
    bY = mulInteger 4 (inv 5)
    bX :: Integer
    bX = xRecover bY

-- Helpers

decodePoint :: ByteString -> (Integer, Integer)
decodePoint s =
  let y = bsToInteger False s
      x = _
      p = (x, y)
   in ite (onCurve p) p (error "decoding point that's not on curve")

encodePoint :: (Integer, Integer) -> ByteString
encodePoint = _

hInt :: ByteString -> Integer
hInt = _

scalarMult :: (Integer, Integer) -> Integer -> Integer
scalarMult = _

edwards :: (Integer, Integer) -> Integer -> Integer
edwards = _

inv :: Integer -> Integer
inv = _

xRecover :: Integer -> Integer
xRecover = _

onCurve :: (Integer, Integer) -> Bool
onCurve = _
