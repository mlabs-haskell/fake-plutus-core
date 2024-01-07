{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SHA512 (sha512) where

import Bitwise
  ( andBS,
    broadcastBS,
    orBS,
    rotateBS,
    shiftBS,
    xorBS,
  )
import Control.Category ((.))
import Core
  ( addInteger,
    appendBS,
    bsToInteger,
    consBS,
    consList,
    emptyList,
    integerToBS,
    ite,
    leInteger,
    lenBS,
    ltInteger,
    mulInteger,
    quotInteger,
    sliceBS,
    sndPair,
    subInteger,
  )
import Data.Bool (Bool (True))
import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Kind (Type)
import Derived (and, foldl)
import GHC.Num (Integer)

-- Based on https://github.com/haskell-hvr/cryptohash-sha512/blob/master/cbits/hs_sha512.h
sha512 :: ByteString -> ByteString
sha512 input = ctxFinalize (ctxUpdate ctxInit input)

-- Helpers

newtype UInt64 = UInt64 ByteString

toBS :: UInt64 -> ByteString
toBS (UInt64 bs) = bs

toInteger :: UInt64 -> Integer
toInteger (UInt64 x) = bsToInteger True x

unsafeFromInteger :: Integer -> UInt64
unsafeFromInteger = UInt64 . integerToBS True 8

unsafeFromBS :: ByteString -> UInt64
unsafeFromBS = UInt64

addUInt64 :: UInt64 -> UInt64 -> UInt64
addUInt64 x y =
  let xI = toInteger x
      yI = toInteger y
      added = addInteger xI yI
   in unsafeFromInteger
        . ite (ltInteger limit added) (subInteger (subInteger added limit) 1)
        $ added

shiftUInt64 :: Integer -> UInt64 -> UInt64
shiftUInt64 shift (UInt64 x) = UInt64 . shiftBS shift $ x

andUInt64 :: UInt64 -> UInt64 -> UInt64
andUInt64 (UInt64 x) (UInt64 y) = UInt64 (andBS 8 x y)

orUInt64 :: UInt64 -> UInt64 -> UInt64
orUInt64 (UInt64 x) (UInt64 y) = UInt64 (orBS 8 x y)

xorUInt64 :: UInt64 -> UInt64 -> UInt64
xorUInt64 (UInt64 x) (UInt64 y) = UInt64 (xorBS 8 x y)

rotateUInt64 :: Integer -> UInt64 -> UInt64
rotateUInt64 rotation (UInt64 x) = UInt64 . rotateBS rotation $ x

newtype H = H (UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64, UInt64)

getAsBS :: H -> ByteString
getAsBS (H (x1, x2, x3, x4, x5, x6, x7, x8)) =
  appendBS (toBS x1)
    . appendBS (toBS x2)
    . appendBS (toBS x3)
    . appendBS (toBS x4)
    . appendBS (toBS x5)
    . appendBS (toBS x6)
    . appendBS (toBS x7)
    . toBS
    $ x8

data Ctx = Ctx
  { sz :: UInt64,
    sz_hi :: UInt64,
    buf :: ByteString,
    h :: H
  }

getSz :: Ctx -> UInt64
getSz = sz

limit :: Integer
limit = 18_446_744_073_709_551_615

getIndex :: Ctx -> Integer
getIndex = toInteger . andUInt64 (unsafeFromInteger 0x7F) . getSz

getSzHi :: Ctx -> UInt64
getSzHi = sz_hi

getH :: Ctx -> H
getH = h

ctxFinalize :: Ctx -> ByteString
ctxFinalize ctx =
  let padding = consBS 0x80 . broadcastBS 127 $ 0x00
      bitsFst = orUInt64 (shiftUInt64 3 . getSzHi $ ctx) (shiftUInt64 (-61) . getSz $ ctx)
      bitsSnd = shiftUInt64 3 . getSz $ ctx
      index = getIndex ctx
      padlen = ite (ltInteger index 112) (subInteger 112 index) (subInteger 240 index)
      ctx1 = ctxUpdate ctx . sliceBS padding 0 $ padlen
      ctx2 = ctxUpdate ctx1 . appendBS (toBS bitsFst) . toBS $ bitsSnd
   in getAsBS . getH $ ctx2

ctxInit :: Ctx
ctxInit =
  Ctx
    { sz = unsafeFromInteger 0x00,
      sz_hi = unsafeFromInteger 0x00,
      buf = broadcastBS 128 0x00,
      h =
        H
          ( unsafeFromInteger 0x6a09_e667_f3bc_c908,
            unsafeFromInteger 0xbb67_ae85_84ca_a73b,
            unsafeFromInteger 0x3c6e_f372_fe94_f82b,
            unsafeFromInteger 0xa54f_f53a_5f1d_36f1,
            unsafeFromInteger 0x510e_527f_ade6_82d1,
            unsafeFromInteger 0x9b05_688c_2b3e_6c1f,
            unsafeFromInteger 0x1f83_d9ab_fb41_bd6b,
            unsafeFromInteger 0x5be0_cd19_137e_2179
          )
    }

ctxUpdate :: Ctx -> ByteString -> Ctx
ctxUpdate ctx input =
  let index = getIndex ctx
      toFill = subInteger 128 index
      len = lenBS input
      havePartialBuffer = and (ltInteger 0 index) (leInteger toFill len)
      dat = input -- we'll slice it later
      (ctx1, len1, data1, index1) =
        ite
          havePartialBuffer
          ( writeToBuffer ctx index dat toFill,
            subInteger len toFill,
            sliceBS dat toFill (subInteger (lenBS dat) toFill),
            0
          )
          (ctx, len, dat, index)
      chunksToDo = quotInteger len1 128
      (ctx2, len2, data2) = foldl go (ctx1, len1, data1) . repeat chunksToDo $ ()
   in writeToBuffer ctx2 index1 data2 len2
  where
    go :: (Ctx, Integer, ByteString) -> () -> (Ctx, Integer, ByteString)
    go (ctx', len', data') _ =
      let ctx'' = doChunk ctx' data'
          len'' = subInteger len' 128
          data'' = sliceBS data' 128 (subInteger (lenBS data') 128)
       in (ctx'', len'', data'')

writeToBuffer :: Ctx -> Integer -> ByteString -> Integer -> Ctx
writeToBuffer ctx index dat len =
  let prefix = sliceBS (buf ctx) 0 index
      suffix = sliceBS dat 0 len
   in ctx {buf = appendBS prefix suffix}

repeat :: forall (a :: Type). Integer -> a -> [a]
repeat i x = ite (ltInteger i 0) emptyList (consList x . repeat (subInteger i 1) $ x)

doChunk :: Ctx -> ByteString -> Ctx
doChunk ctx input =
  let w = appendBS (sliceBS input 0 128) (broadcastBS 512 0x00)
      w1 = foldl go w . fromToStepList 16 80 $ 1
      ctxH@(H (a, b, c, d, e, f, g, h')) = getH ctx
      H (a', b', c', d', e', f', g', h'') = foldl (go2 w1) ctxH . fromToStepList 0 79 $ 1
   in ctx
        { h =
            H
              ( addUInt64 a a',
                addUInt64 b b',
                addUInt64 c c',
                addUInt64 d d',
                addUInt64 e e',
                addUInt64 f f',
                addUInt64 g g',
                addUInt64 h' h''
              )
        }
  where
    go :: ByteString -> Integer -> ByteString
    go curr uint64Ix =
      let wIMinus2 = indexBSUInt64 curr (subInteger uint64Ix 2)
          wIMinus7 = indexBSUInt64 curr (subInteger uint64Ix 7)
          wIMinus15 = indexBSUInt64 curr (subInteger uint64Ix 15)
          wIMinus16 = indexBSUInt64 curr (subInteger uint64Ix 16)
          withS1 = s1 wIMinus2
          withS0 = s0 wIMinus15
          added = addUInt64 withS1 (addUInt64 wIMinus7 (addUInt64 withS0 wIMinus16))
       in writeBSUInt64 curr uint64Ix added
    go2 :: ByteString -> H -> Integer -> H
    go2 w (H (a, b, c, d, e, f, g, h')) i1 =
      let (d1, h1) = r a b c d e f g h' (indexBSUInt64 w i1) (indexBSUInt64 k i1)
          i2 = addInteger i1 1
          (c1, g1) = r h1 a b c d1 e f g (indexBSUInt64 w i2) (indexBSUInt64 k i2)
          i3 = addInteger i2 1
          (b1, f1) = r g1 h1 a b c1 d1 e f (indexBSUInt64 w i3) (indexBSUInt64 k i3)
          i4 = addInteger i3 1
          (a1, e1') = r f1 g1 h1 a b1 c1 d1 e (indexBSUInt64 w i4) (indexBSUInt64 k i4)
          i5 = addInteger i4 1
          (h2, d2) = r e1' f1 g1 h1 a1 b1 c1 d1 (indexBSUInt64 w i5) (indexBSUInt64 k i5)
          i6 = addInteger i5 1
          (g2, c2) = r d2 e1' f1 g1 h2 a1 b1 c1 (indexBSUInt64 w i6) (indexBSUInt64 k i6)
          i7 = addInteger i6 1
          (f2, b2) = r c2 d2 e1' f1 g2 h2 a1 b1 (indexBSUInt64 w i7) (indexBSUInt64 k i7)
          i8 = addInteger i7 1
          (e2, a2) = r b2 c2 d2 e1' f2 g2 h2 a1 (indexBSUInt64 w i8) (indexBSUInt64 k i8)
       in H (a2, b2, c2, d2, e2, f2, g2, h2)

fromToStepList :: Integer -> Integer -> Integer -> [Integer]
fromToStepList start end step = go start
  where
    go :: Integer -> [Integer]
    go i = ite (ltInteger end i) emptyList (consList i (go (addInteger i step)))

s0 :: UInt64 -> UInt64
s0 x = xorUInt64 (rotateUInt64 (-1) x) (xorUInt64 (rotateUInt64 (-8) x) (shiftUInt64 (-7) x))

s1 :: UInt64 -> UInt64
s1 x = xorUInt64 (rotateUInt64 (-19) x) (xorUInt64 (rotateUInt64 (-61) x) (shiftUInt64 (-6) x))

indexBSUInt64 :: ByteString -> Integer -> UInt64
indexBSUInt64 bs uint64Ix =
  let byteIx = mulInteger uint64Ix 8
      sliced = sliceBS bs byteIx 8
   in unsafeFromBS sliced

writeBSUInt64 :: ByteString -> Integer -> UInt64 -> ByteString
writeBSUInt64 bs uint64Ix val =
  let byteIx = mulInteger uint64Ix 8
      len = lenBS bs
      asBS = toBS val
      prefix = sliceBS bs 0 byteIx
      suffix = sliceBS bs (addInteger byteIx 8) (subInteger len (addInteger byteIx 8))
   in appendBS prefix (appendBS asBS suffix)

k :: ByteString
k =
  sndPair
    . foldl
      (\(i, acc) x -> (addInteger i 1, writeBSUInt64 acc i . unsafeFromInteger $ x))
      (0, broadcastBS 640 0x00)
    $ kList
  where
    kList :: [Integer]
    kList =
      [ 0x428a_2f98_d728_ae22,
        0x7137_4491_23ef_65cd,
        0xb5c0_fbcf_ec4d_3b2f,
        0xe9b5_dba5_8189_dbbc,
        0x3956_c25b_f348_b538,
        0x59f1_11f1_b605_d019,
        0x923f_82a4af194f9b,
        0xab1c_5ed5da6d8118,
        0xd807_aa98a3030242,
        0x1283_5b0145706fbe,
        0x2431_85be4ee4b28c,
        0x550c_7dc3d5ffb4e2,
        0x72be_5d74f27b896f,
        0x80de_b1fe3b1696b1,
        0x9bdc_06a725c71235,
        0xc19b_f174cf692694,
        0xe49b_69c19ef14ad2,
        0xefbe_4786384f25e3,
        0x0fc1_9dc68b8cd5b5,
        0x240c_a1cc77ac9c65,
        0x2de9_2c6f592b0275,
        0x4a74_84aa6ea6e483,
        0x5cb0_a9dcbd41fbd4,
        0x76f9_88da831153b5,
        0x983e_5152ee66dfab,
        0xa831_c66d2db43210,
        0xb003_27c898fb213f,
        0xbf59_7fc7beef0ee4,
        0xc6e0_0bf33da88fc2,
        0xd5a7_9147930aa725,
        0x06ca_6351e003826f,
        0x1429_29670a0e6e70,
        0x27b7_0a8546d22ffc,
        0x2e1b_21385c26c926,
        0x4d2c_6dfc5ac42aed,
        0x5338_0d139d95b3df,
        0x650a_73548baf63de,
        0x766a_0abb3c77b2a8,
        0x81c2_c92e47edaee6,
        0x9272_2c851482353b,
        0xa2bf_e8a14cf10364,
        0xa81a_664bbc423001,
        0xc24b_8b70d0f89791,
        0xc76c_51a30654be30,
        0xd192_e819d6ef5218,
        0xd699_06245565a910,
        0xf40e_35855771202a,
        0x106a_a07032bbd1b8,
        0x19a4_c116b8d2d0c8,
        0x1e37_6c085141ab53,
        0x2748_774cdf8eeb99,
        0x34b0_bcb5e19b48a8,
        0x391c_0cb3c5c95a63,
        0x4ed8_aa4ae3418acb,
        0x5b9c_ca4f7763e373,
        0x682e_6ff3d6b2b8a3,
        0x748f_82ee5defb2fc,
        0x78a5_636f43172f60,
        0x84c8_7814a1f0ab72,
        0x8cc7_02081a6439ec,
        0x90be_fffa23631e28,
        0xa450_6cebde82bde9,
        0xbef9_a3f7b2c67915,
        0xc671_78f2e372532b,
        0xca27_3eceea26619c,
        0xd186_b8c721c0c207,
        0xeada_7dd6cde0eb1e,
        0xf57d_4f7fee6ed178,
        0x06f0_67aa72176fba,
        0x0a63_7dc5a2c898a6,
        0x113f_9804bef90dae,
        0x1b71_0b35131c471b,
        0x28db_77f523047d84,
        0x32ca_ab7b40c72493,
        0x3c9e_be0a15c9bebc,
        0x431d_67c49c100d4c,
        0x4cc5_d4becb3e42b6,
        0x597f_299cfc657e2a,
        0x5fcb_6fab3ad6faec,
        0x6c44_198c4a475817
      ]

r ::
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  UInt64 ->
  (UInt64, UInt64)
r a b c d e f g h' k' w =
  let e1e = e1 e
      fXorG = xorUInt64 f g
      eAndFXorG = andUInt64 e fXorG
      gXorEAndFXorG = xorUInt64 g eAndFXorG
      t1 = addUInt64 h' (addUInt64 e1e (addUInt64 gXorEAndFXorG (addUInt64 k' w)))
      e0a = e0 a
      aOrB = orUInt64 a b
      cAndAOrB = andUInt64 c aOrB
      aAndB = andUInt64 a b
      t2 = addUInt64 e0a (orUInt64 aAndB cAndAOrB)
      d' = addUInt64 d t1
      h'' = addUInt64 t1 t2
   in (d', h'')

e1 :: UInt64 -> UInt64
e1 x = xorUInt64 (rotateUInt64 (-14) x) (xorUInt64 (rotateUInt64 (-18) x) (rotateUInt64 (-41) x))

e0 :: UInt64 -> UInt64
e0 x = xorUInt64 (rotateUInt64 (-28) x) (xorUInt64 (rotateUInt64 (-34) x) (rotateUInt64 (-39) x))
