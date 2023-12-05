{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Baseline.UInt64 qualified as Baseline
import Control.Category ((.))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Kind (Type)
import Data.Ord ((<))
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.Integer (Integer)
import GHC.Num (abs)
import Optimized.UInt64 qualified as Optimized
import System.IO (IO)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    env,
    nf,
  )
import Text.Show (show)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "and"
        [ andBaseline,
          andOptimized
        ],
      bgroup
        "overflow-checking add"
        [ addNoOFBaseline,
          addNoOFOptimized,
          addOFBaseline,
          addOFOptimized
        ],
      bgroup
        "shift"
        [ bgroup "baseline" . fmap (mkShift Baseline.toUInt64 Baseline.shiftUInt64) $ sizes,
          bgroup "bitwise" . fmap (mkShift Optimized.toUInt64 Optimized.shiftUInt64) $ sizes
        ],
      bgroup
        "rotate"
        [ bgroup "baseline" . fmap (mkRotate Baseline.toUInt64 Baseline.rotateUInt64) $ sizes,
          bgroup "bitwise" . fmap (mkRotate Optimized.toUInt64 Optimized.rotateUInt64) $ sizes
        ]
    ]

-- Helpers

sizes :: [Integer]
sizes = [(-65), (-64) .. 65]

addNoOFBaseline :: Benchmark
addNoOFBaseline =
  let x = Baseline.toUInt64 55
      y = Baseline.toUInt64 66
   in bench "baseline, no overflow" $ nf (Baseline.addOverflowUInt64 x) y

addNoOFOptimized :: Benchmark
addNoOFOptimized =
  let x = Optimized.toUInt64 55
      y = Optimized.toUInt64 66
   in bench "bitwise, no overflow" $ nf (Optimized.addOverflowUInt64 x) y

addOFBaseline :: Benchmark
addOFBaseline =
  let x = Baseline.toUInt64 18_446_744_073_709_551_615
      y = Baseline.toUInt64 66
   in bench "baseline, overflow" $ nf (Baseline.addOverflowUInt64 x) y

addOFOptimized :: Benchmark
addOFOptimized =
  let x = Optimized.toUInt64 18_446_744_073_709_551_615
      y = Optimized.toUInt64 66
   in bench "bitwise, overflow" $ nf (Optimized.addOverflowUInt64 x) y

andBaseline :: Benchmark
andBaseline =
  let x = Baseline.toUInt64 55
      y = Baseline.toUInt64 66
   in bench "baseline" $ nf (Baseline.andUInt64 x) y

andOptimized :: Benchmark
andOptimized =
  let x = Optimized.toUInt64 55
      y = Optimized.toUInt64 66
   in bench "bitwise" $ nf (Optimized.andUInt64 x) y

mkShift ::
  forall (a :: Type).
  NFData a =>
  (Integer -> a) ->
  (a -> Integer -> a) ->
  Integer ->
  Benchmark
mkShift mk op shift =
  env (evaluate . force $ mkData) $ \dat ->
    bench benchName $ nf (op dat) shift
  where
    mkData :: a
    mkData = mk 55
    benchName :: String
    benchName =
      if shift < 0
        then "right shift by " <> show (abs shift)
        else "left shift by " <> show shift

mkRotate ::
  forall (a :: Type).
  NFData a =>
  (Integer -> a) ->
  (a -> Integer -> a) ->
  Integer ->
  Benchmark
mkRotate mk op rotation =
  env (evaluate . force $ mkData) $ \dat ->
    bench benchName $ nf (op dat) rotation
  where
    mkData :: a
    mkData = mk 55
    benchName :: String
    benchName =
      if rotation < 0
        then "right rotation by " <> show (abs rotation)
        else "left rotation by " <> show rotation
