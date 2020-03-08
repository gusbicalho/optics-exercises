{-# LANGUAGE NoOverloadedStrings #-}
module Isos where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Functor
import Data.List.Lens (prefixed)
import Data.Monoid
import qualified Data.List as L
import Data.List (transpose)
import qualified Data.Ord as Ord
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Numeric.Lens
import Text.Read (readMaybe)

check :: Eq b => (b, b) -> Maybe (b, b)
check (actual, expected)
  | actual == expected = Nothing
  | otherwise = Just (actual, expected)

switchCase :: Char -> Char
switchCase c = if isUpper c then toLower c else toUpper c

exIsos2 =
  ( check ( ("Beauty", "Age") ^. swapped
          , ("Age", "Beauty")
          )
  , check ( 50 ^. from (adding 10)
          , 40
          )
  , check ( 0 & multiplying 4 +~ 12
          , 3.0
          )
  , check ( 0 & adding 10 . multiplying 2 .~ 24
          , 2
          )
  , check ( [1, 2, 3] & reversed %~ drop 1
          , [1, 2]
          )
  , check ( (view flipped (++)) [1, 2] [3, 4]
          , [3,4,1,2]
          )
  , check ( [1, 2, 3] ^. reversed
          , [3, 2, 1]
          )
  , check ( [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ drop 1
          , [[2,3],[20,30]]
          )
  , check ( (32, "Hi") & _2 . involuted (fmap switchCase) .~ ("hELLO" :: String)
          , (32,"Hello")
          )
  )

fahrenheit :: Iso' Double Double
fahrenheit = multiplying 9 . dividing 5 . adding 32

exProjectedIsos1 =
  ( check ( ("Beauty", "Age") ^. mapping reversed . swapped
          , ("egA","Beauty")
          )
  , check ( [True, False, True] ^. mapping (involuted not)
          , [False,True,False]
          )
  , check ( [True, False, True] & mapping (involuted not) %~ filter id
          , [False]
          )
  , check ( (show ^. mapping reversed) 1234
          , "4321"
          )
  )

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)
-- intNot = enum %~ not
