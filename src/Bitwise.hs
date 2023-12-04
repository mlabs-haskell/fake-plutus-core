{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bitwise
  ( andBS,
    shiftBS,
    rotateBS,
  )
where

import Control.Monad (unless)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.Bool (otherwise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Eq ((==))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Int (Int)
import Data.Ord (min, (<), (<=), (>=))
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Num (Integer, abs, (*), (+), (-))
import GHC.Real (fromIntegral, quotRem, rem)

-- Similar convention to conversions: if we request a specific size, that's what
-- we get, but if not, we pad to whichever is longer.
andBS :: Integer -> ByteString -> ByteString -> ByteString
andBS requestedLength bs1 bs2
  | requestedLength <= 0 =
      if BS.length bs1 < BS.length bs2
        then andNoLimit bs1 bs2
        else andNoLimit bs2 bs1
  | otherwise =
      let len = fromIntegral requestedLength
       in if BS.length bs1 < BS.length bs2
            then andLimit len bs1 bs2
            else andLimit len bs2 bs1
  where
    andNoLimit :: ByteString -> ByteString -> ByteString
    andNoLimit shorter longer = BSI.unsafeCreate (BS.length longer) $ \dstPtr ->
      BS.useAsCStringLen shorter $ \(shorterSrcPtr, shorterLen) ->
        BS.useAsCStringLen longer $ \(longerSrcPtr, longerLen) -> do
          copyBytes (castPtr dstPtr) longerSrcPtr longerLen
          for_ [0 .. shorterLen - 1] $ \i -> do
            srcByte :: Word8 <- peekByteOff shorterSrcPtr i
            dstByte :: Word8 <- peekByteOff dstPtr i
            pokeByteOff dstPtr i (srcByte .&. dstByte)
    andLimit :: Int -> ByteString -> ByteString -> ByteString
    andLimit limit shorter longer = BSI.unsafeCreate limit $ \dstPtr ->
      BS.useAsCStringLen shorter $ \(shorterSrcPtr, shorterLen) ->
        BS.useAsCString longer $ \longerSrcPtr -> do
          fillBytes dstPtr 0xff limit
          copyBytes (castPtr dstPtr) longerSrcPtr limit
          for_ [0 .. min shorterLen limit - 1] $ \i -> do
            srcByte :: Word8 <- peekByteOff shorterSrcPtr i
            dstByte :: Word8 <- peekByteOff dstPtr i
            pokeByteOff dstPtr i (srcByte .&. dstByte)

-- We follow the principle that bits are numbered starting from the end. A shift
-- is an index map: positive means indexes map upward (aka left shift), negative
-- means they map downward (aka right shift).
shiftBS :: Integer -> ByteString -> ByteString
shiftBS shift bs
  | shift == 0 = bs
  | otherwise =
      let len = BS.length bs
          bitLen = 8 * len
          shift' = fromIntegral shift
          absShift = abs shift'
       in if
              | len == 0 -> BS.empty
              | absShift >= bitLen -> BS.replicate len 0x00
              | shift' < 0 -> leftShift len absShift
              | otherwise -> rightShift len absShift
  where
    leftShift :: Int -> Int -> ByteString
    leftShift len bitShift = BSI.unsafeCreate len $ \dstPtr ->
      BS.useAsCString bs $ \srcPtr -> do
        let (byteShift, remShift) = bitShift `quotRem` 8
        fillBytes dstPtr 0x00 len
        -- First, remap individual source bytes by byteShift to the left
        -- (meaning, indexes decrease). Thus, dstPtr 0 is srcPtr byteShift,
        -- dstPtr 1 is srcPtr byteShift + 1, etc.
        for_ [byteShift, byteShift + 1 .. len - 1] $ \srcIx -> do
          srcByte :: Word8 <- peekByteOff srcPtr srcIx
          pokeByteOff dstPtr (srcIx - byteShift) srcByte
        -- If we have any 'leftover bits', we need to shift each byte that much,
        -- then 'borrow' some bits from the byte next to it. For the last byte,
        -- we borrow zeroes.
        unless (remShift == 0) $ do
          for_ [0 .. len - 2] $ \dstIx -> do
            currByte :: Word8 <- peekByteOff dstPtr dstIx
            nextByte :: Word8 <- peekByteOff dstPtr (dstIx + 1)
            let newCurrByte = (currByte `unsafeShiftL` remShift) .|. (nextByte `unsafeShiftR` (8 - remShift))
            pokeByteOff dstPtr dstIx newCurrByte
          -- Do the last byte with zeroes
          lastByte :: Word8 <- peekByteOff dstPtr (len - 1)
          let newLastByte = lastByte `unsafeShiftL` remShift
          pokeByteOff dstPtr (len - 1) newLastByte
    rightShift :: Int -> Int -> ByteString
    rightShift len bitShift = BSI.unsafeCreate len $ \dstPtr ->
      BS.useAsCString bs $ \srcPtr -> do
        let (byteShift, remShift) = bitShift `quotRem` 8
        fillBytes dstPtr 0x00 len
        -- First, remap individual source bytes by byteShift to the right
        -- (meaning, indexes increase). Thus, dstPtr byteShift is srcPtr 0,
        -- dstPtr byteShift + 1 is srcPtr 1, etc.
        for_ [byteShift, byteShift + 1 .. len - 1] $ \dstIx -> do
          srcByte :: Word8 <- peekByteOff srcPtr (dstIx - byteShift)
          pokeByteOff dstPtr dstIx srcByte
        -- If we have any 'leftover bits', we need to shift each byte that much,
        -- then borrow some bits from the byte before it. For the first byte, we
        -- borrow zeroes.
        unless (remShift == 0) $ do
          for_ [len - 1, len - 2 .. 1] $ \dstIx -> do
            currByte :: Word8 <- peekByteOff dstPtr dstIx
            prevByte :: Word8 <- peekByteOff dstPtr (dstIx - 1)
            let newCurrByte = (currByte `unsafeShiftR` remShift) .|. (prevByte `unsafeShiftL` (8 - remShift))
            pokeByteOff dstPtr dstIx newCurrByte
          -- Do the first byte with zeroes
          firstByte :: Word8 <- peekByteOff dstPtr 0
          let newFirstByte = firstByte `unsafeShiftR` remShift
          pokeByteOff dstPtr 0 newFirstByte

-- Same as for shiftBS.
rotateBS :: Integer -> ByteString -> ByteString
rotateBS rotation bs
  | rotation == 0 = bs
  | otherwise =
      let len = BS.length bs
          bitLen = 8 * len
          rotation' = fromIntegral rotation
          absRotation = abs rotation'
          reducedRotation = absRotation `rem` bitLen
       in if
              | len == 0 -> BS.empty
              | absRotation == 0 -> bs
              | rotation' < 0 -> leftRotation len reducedRotation
              | otherwise -> rightRotation len reducedRotation
  where
    leftRotation :: Int -> Int -> ByteString
    leftRotation len bitRotation = BSI.unsafeCreate len $ \dstPtr ->
      BS.useAsCString bs $ \srcPtr -> do
        let (byteRotation, remRotation) = bitRotation `quotRem` 8
        -- We do a similar remapping as for left shifts, except now, we remap
        -- _every_ byte, and wrap around if our remapping would put us out of
        -- bounds.
        for_ [0 .. len - 1] $ \srcIx -> do
          let correspondingIx = (srcIx + len - byteRotation) `rem` len
          srcByte :: Word8 <- peekByteOff srcPtr srcIx
          pokeByteOff dstPtr correspondingIx srcByte
        -- If we have any 'leftover bits', we need to rotate each byte one at a
        -- time, by 'borrowing' some bits from the byte next to it, wrapping
        -- around from last to first.
        unless (remRotation == 0) $ do
          firstByte :: Word8 <- peekByteOff dstPtr 0
          for_ [0 .. len - 2] $ \dstIx -> do
            currByte :: Word8 <- peekByteOff dstPtr dstIx
            nextByte :: Word8 <- peekByteOff dstPtr (dstIx + 1)
            let newCurrByte = (currByte `unsafeShiftL` remRotation) .|. (nextByte `unsafeShiftR` (8 - remRotation))
            pokeByteOff dstPtr dstIx newCurrByte
          lastByte :: Word8 <- peekByteOff dstPtr (len - 1)
          let newLastByte = (lastByte `unsafeShiftL` remRotation) .|. (firstByte `unsafeShiftR` (8 - remRotation))
          pokeByteOff dstPtr (len - 1) newLastByte
    rightRotation :: Int -> Int -> ByteString
    rightRotation len bitRotation = BSI.unsafeCreate len $ \dstPtr ->
      BS.useAsCString bs $ \srcPtr -> do
        let (byteRotation, remRotation) = bitRotation `quotRem` 8
        -- We do a similar remapping as for right shifts, except now, we remap
        -- _every_ byte, and wrap around if our remapping would put us out of
        -- bounds.
        for_ [0 .. len - 1] $ \srcIx -> do
          let correspondingIx = (srcIx + byteRotation) `rem` len
          srcByte :: Word8 <- peekByteOff srcPtr srcIx
          pokeByteOff dstPtr correspondingIx srcByte
        -- If we have any 'leftover bits', we need to rotate each byte one at a
        -- time, by 'borrowing' some bits from the byte preceding it, wrapping
        -- around from first to last.
        unless (remRotation == 0) $ do
          lastByte :: Word8 <- peekByteOff dstPtr (len - 1)
          for_ [len - 1, len - 2 .. 1] $ \dstIx -> do
            currByte :: Word8 <- peekByteOff dstPtr dstIx
            prevByte :: Word8 <- peekByteOff dstPtr (dstIx - 1)
            let newCurrByte = (currByte `unsafeShiftR` remRotation) .|. (prevByte `unsafeShiftL` (8 - remRotation))
            pokeByteOff dstPtr dstIx newCurrByte
          firstByte :: Word8 <- peekByteOff dstPtr 0
          let newFirstByte = (firstByte `unsafeShiftR` remRotation) .|. (lastByte `unsafeShiftL` (8 - remRotation))
          pokeByteOff dstPtr 0 newFirstByte