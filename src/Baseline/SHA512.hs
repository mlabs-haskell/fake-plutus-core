{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Baseline.SHA512 (sha512) where

import Baseline.UInt64
  ( UInt64,
    andUInt64,
    fromUInt64,
    toUInt64,
  )
import Core
  ( addInteger,
    appendBS,
    consBS,
    ite,
    leInteger,
    lenBS,
    ltInteger,
    mulInteger,
    quotInteger,
    sliceBS,
    subInteger,
  )
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Num (Integer)

-- Based on https://github.com/haskell-hvr/cryptohash-sha512/blob/master/cbits/hs_sha512.h
sha512 :: ByteString -> ByteString
sha512 input = sha512CtxFinalize (sha512CtxUpdate sha512CtxInit input)

-- Helpers

data SHA512Ctx = SHA512Ctx
  { sz :: UInt64,
    sz_hi :: UInt64,
    buf :: ByteString,
    h :: [UInt64]
  }

sha512CtxInit :: SHA512Ctx
sha512CtxInit =
  SHA512Ctx
    { sz = 0,
      sz_hi = 0,
      -- Technically we have no such primitive, but this is technically a constant,
      -- so it could be written as one (it's just tedious).
      buf = BS.replicate 128 0x00,
      h =
        [ toUInt64 0x6a09_e667_f3bc_c908,
          toUInt64 0xbb67_ae85_84ca_a73b,
          toUInt64 0x3c6e_f372_fe94_f82b,
          toUInt64 0xa54f_f53a_5f1d_36f1,
          toUInt64 0x510e_527f_ade6_82d1,
          toUInt64 0x9b05_688c_2b3e_6c1f,
          toUInt64 0x1f83_d9ab_fb41_bd6b,
          toUInt64 0x5be0_cd19_137e_2179
        ]
    }

sha512CtxUpdate :: SHA512Ctx -> ByteString -> SHA512Ctx
sha512CtxUpdate ctx bs =
  let sz1 = sz ctx
      sz_hi1 = sz_hi ctx
      index = andUInt64 sz1 (toUInt64 0x7f)
      toFill = subInteger 128 (fromUInt64 index)
      len = lenBS bs
      (sz2, sz_hi2) = addWithOverflow sz1 len sz_hi1
      ctx2 = ctx {sz = sz2, sz_hi = sz_hi2}
      dataIx = 0
      (ctx3, len2, dataIx1) =
        ite
          (and (ltInteger 0 index) (leInteger toFill len))
          (rollInPartial ctx2 len dataIx bs index toFill)
          (ctx2, len, dataIx)
      (ctx4, len3, dataIx2) = rollInFull ctx3 len2 dataIx1 bs
      buf1 = buf ctx4
      buf2 = ite (ltInteger 0 len3) _ buf1
   in ctx4 {buf = buf2}
  where
    rollInPartial ::
      SHA512Ctx ->
      Integer ->
      Integer ->
      ByteString ->
      Integer ->
      Integer ->
      (SHA512Ctx, Integer, Integer)
    rollInPartial ctx' len' dataIx' bs' index' toFill' =
      let prefix = sliceBS (buf ctx') index' toFill'
          suffix = sliceBS bs' dataIx' toFill'
          combined = appendBS prefix suffix
          ctx1' = sha512DoChunk ctx' bs' dataIx'
       in (ctx1', subInteger len' toFill', addInteger dataIx' toFill')
    rollInFull ::
      SHA512Ctx ->
      Integer ->
      Integer ->
      ByteString ->
      (SHA512Ctx, Integer, Integer)
    rollInFull ctx' len' dataIx' bs' = _

sha512CtxFinalize :: SHA512Ctx -> ByteString
sha512CtxFinalize ctx =
  let -- Once again, we don't have such a primitive, but since this is a constant,
      -- we could write it as one, it's just very tedious.
      padding = consBS 0x80 (BS.replicate 127 0x00)
   in _

sha512DoChunk :: SHA512Ctx -> ByteString -> Integer -> SHA512Ctx
sha512DoChunk = _

addWithOverflow :: Integer -> Integer -> Integer -> (Integer, Integer)
addWithOverflow x y overflow =
  let added = addInteger x y
   in ite
        (leInteger limit added)
        (quotInteger added limit, addInteger overflow 1)
        (added, overflow)

limit :: Integer
limit = 18_446_744_073_709_551_616
