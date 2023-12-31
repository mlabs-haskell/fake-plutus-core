{-# LANGUAGE NoImplicitPrelude #-}

module Optimized.NQueens (Position, nqueens) where

import Bitwise
  ( andBS,
    broadcastBS,
    selectBS,
    setBit,
    shiftBS,
  )
import Core
  ( addInteger,
    consList,
    emptyList,
    eqInteger,
    ite,
    nullList,
    quotInteger,
    subInteger,
  )
import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString)
import GHC.Num (Integer)

-- Based on Qiu, Zongyan (February 2002). "Bit-vector encoding of n-queen problem". ACM SIGPLAN Notices. 37 (2): 68–70
-- For simplicity, we assume whole byte multiples (8, 16, 24) as values of n
nqueens :: Integer -> [Position]
nqueens n =
  let down = broadcastBS bytesNeeded 0xFF
      left = broadcastBS bytesNeeded 0xFF
      right = broadcastBS bytesNeeded 0xFF
   in go 0 0 down left right
  where
    go ::
      Integer ->
      Integer ->
      ByteString ->
      ByteString ->
      ByteString ->
      [Position]
    go selectIx row down left right =
      ite
        (eqInteger selectIx n)
        emptyList
        ( let opts = andBS bytesNeeded down (andBS bytesNeeded left right)
              available = selectBS selectIx True opts
           in ite
                (eqInteger available (-1))
                emptyList
                ( ite
                    (eqInteger row lastRow)
                    (consList (Position row available) emptyList)
                    ( let newDown = setBit down available False
                          newLeft =
                            setBit
                              (shiftBS 1 (setBit left available False))
                              0
                              True
                          newRight =
                            setBit
                              (shiftBS (-1) (setBit right available False))
                              lastPosition
                              True
                          newRow = addInteger row 1
                          next = go 0 newRow newDown newLeft newRight
                       in ite
                            (nullList next)
                            (go (addInteger selectIx 1) row down left right)
                            (consList (Position row available) next)
                    )
                )
        )
    bytesNeeded :: Integer
    bytesNeeded = quotInteger n 8
    lastRow :: Integer
    lastRow = subInteger n 1
    lastPosition :: Integer
    lastPosition = subInteger n 1

-- row, column
data Position = Position Integer Integer
