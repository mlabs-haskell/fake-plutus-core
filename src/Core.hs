{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core
  ( -- * Primop fakes
    eqInteger,
    addInteger,
    subInteger,
    mulInteger,
    quotInteger,
    remInteger,
    ltInteger,
    leInteger,
    ite,
    fstPair,
    sndPair,
    indexBS,
    lenBS,
    sliceBS,
    appendBS,
    consBS,
    integerToBS,
    bsToInteger,
    chooseList,
    headList,
    tailList,
    consList,
    emptyList,
    eqBS,
    nullList,
  )
where

import Control.Category ((.))
import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Eq ((==))
import Data.Function (($))
import Data.Kind (Type)
import Data.Ord ((<), (<=))
import Data.Semigroup ((<>))
import Data.Tuple (fst, snd)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Num (Integer, (*), (+), (-))
import GHC.Real (fromIntegral, quot, rem)
import Naive (fromByteString, toByteString)

-- Fake primops

nullList ::
  forall (a :: Type).
  [a] ->
  Bool
nullList = \case
  [] -> True
  (_ : _) -> False

chooseList ::
  forall (a :: Type) (b :: Type).
  [a] ->
  b ->
  b ->
  b
chooseList xs whenNil whenCons = case xs of
  [] -> whenNil
  _ -> whenCons

headList ::
  forall (a :: Type).
  [a] ->
  a
headList = \case
  [] -> error "empty list"
  (x : _) -> x

tailList ::
  forall (a :: Type).
  [a] ->
  [a]
tailList = \case
  [] -> error "empty list"
  (_ : xs) -> xs

consList ::
  forall (a :: Type).
  a ->
  [a] ->
  [a]
consList = (:)

emptyList ::
  forall (a :: Type).
  [a]
emptyList = []

eqInteger :: Integer -> Integer -> Bool
eqInteger = (==)

quotInteger :: Integer -> Integer -> Integer
quotInteger = quot

remInteger :: Integer -> Integer -> Integer
remInteger = rem

addInteger :: Integer -> Integer -> Integer
addInteger = (+)

mulInteger :: Integer -> Integer -> Integer
mulInteger = (*)

subInteger :: Integer -> Integer -> Integer
subInteger = (-)

ite :: forall (a :: Type). Bool -> a -> a -> a
ite b ifTrue ifFalse = if b then ifTrue else ifFalse

fstPair ::
  forall (a :: Type) (b :: Type).
  (a, b) ->
  a
fstPair = fst

sndPair ::
  forall (a :: Type) (b :: Type).
  (a, b) ->
  b
sndPair = snd

indexBS :: ByteString -> Integer -> Integer
indexBS bs = fromIntegral . BS.index bs . fromIntegral

lenBS :: ByteString -> Integer
lenBS = fromIntegral . BS.length

integerToBS :: Bool -> Integer -> Integer -> ByteString
integerToBS endianness len input =
  let endianness' = if endianness then BigEndian else LittleEndian
   in toByteString (fromIntegral len) endianness' input

bsToInteger :: Bool -> ByteString -> Integer
bsToInteger endianness input =
  let endianness' = if endianness then BigEndian else LittleEndian
   in fromByteString endianness' input

sliceBS :: ByteString -> Integer -> Integer -> ByteString
sliceBS bs start span = BS.take (fromIntegral span) . BS.drop (fromIntegral start) $ bs

appendBS :: ByteString -> ByteString -> ByteString
appendBS bs1 bs2 = bs1 <> bs2

ltInteger :: Integer -> Integer -> Bool
ltInteger = (<)

leInteger :: Integer -> Integer -> Bool
leInteger = (<=)

consBS :: Integer -> ByteString -> ByteString
consBS w8 = BS.cons (fromIntegral w8)

eqBS :: ByteString -> ByteString -> Bool
eqBS = (==)
