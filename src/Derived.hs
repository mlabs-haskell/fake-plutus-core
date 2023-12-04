{-# LANGUAGE NoImplicitPrelude #-}

-- Operations that aren't part of the core, but can be derived from it.
-- Collected here for general use.
module Derived
  ( -- * General
    even,
    powInteger,
    not,
    and,
    negateInteger,
    absInteger,
    minInteger,
  )
where

import Core
  ( eqInteger,
    ite,
    leInteger,
    ltInteger,
    mulInteger,
    quotInteger,
    remInteger,
    subInteger,
  )
import Data.Bool (Bool (False, True))
import GHC.Err (error)
import GHC.Num (Integer)

minInteger :: Integer -> Integer -> Integer
minInteger x y = ite (ltInteger x y) x y

negateInteger :: Integer -> Integer
negateInteger = subInteger 0

absInteger :: Integer -> Integer
absInteger i =
  ite
    (leInteger 0 i)
    i
    (negateInteger i)

even :: Integer -> Bool
even i = eqInteger (remInteger i 2) 0

powInteger :: Integer -> Integer -> Integer
powInteger b e =
  ite
    (ltInteger e 0)
    (error "negative exponent")
    (go 1 e)
  where
    go :: Integer -> Integer -> Integer
    go acc remE =
      ite
        (eqInteger remE 0)
        acc
        ( let newAcc = mulInteger acc acc
              newRemE = quotInteger remE 2
           in ite
                (even remE)
                (go newAcc newRemE)
                (go (mulInteger b newAcc) newRemE)
        )

not :: Bool -> Bool
not b = ite b False True

and :: Bool -> Bool -> Bool
and x y = ite x y x
