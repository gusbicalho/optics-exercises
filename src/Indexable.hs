{-# LANGUAGE NoOverloadedStrings #-}
module Indexable where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Monoid
import qualified Data.List as L
import qualified Data.Ord as Ord
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Read (readMaybe)

check :: Eq b => (b, b) -> Maybe (b, b)
check (actual, expected)
  | actual == expected = Nothing
  | otherwise = Just (actual, expected)

exIndexable1 =
  ( check ( ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"
          , ["Larry","Wiggly","Moe"]
          )
  , check ( let heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]
            in heroesAndVillains & at "Spiderman" .~ Just "Goblin"
          , M.fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]
          )
  , check ( let heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]
            in sans "Superman" heroesAndVillains
          , M.fromList [("Batman","Joker")]
          )
  , check ( S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' ?~ () & at 'i' .~ Nothing
          , S.fromList "aeouy"
          )
  )

exIndexable2 =
  let input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
      output = M.fromList [("candy bars",13),("ice cream",5),("soda",37)]
  in check ( input & ix "soda" .~ 37 & at "gum" .~ Nothing & at "ice cream" ?~ 5
           , output
           )

newtype Cycled x = Cycled [x]
  deriving (Eq, Show)

type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  ix i handler (Cycled as) = Cycled <$> (as & ix cappedI %%~ handler)
    where cappedI = i `mod` length as

--  Write an optic which focuses the value at key “first” or, failing that, the value at key “second”.
exMissingValues1 =
  let optic = ix "first" `failing` ix "second"
  in  ( check ( M.fromList [("first", False), ("second", False)] & optic .~ True
              , M.fromList [("first",True),("second",False)]
              )
      , check ( M.fromList [("second", False)] & optic .~ True
              , M.fromList [("second",True)]
              )
      )

-- Write an optic which focuses the first element of a tuple iff it is even, and
-- the second tuple element otherwise. Assume each slot contains an integer.
exMissingValues2 =
  let optic = _1 . filtered even `failing` _2
  in  ( check ( (1, 1) & optic *~ 10
              , (1,10)
              )
      , check ( (2, 2) & optic *~ 10
              , (20, 2)
              )
      )

exMissingValues3 =
  let optic = traversed . (filtered even `failing` id)
  in  ( check ( [1,2,3,4] ^.. optic
              , [2,4]
              )
      , check ( [1,3,5] ^.. optic
              , [1,3,5]
              )
      )

exMissingValues4 =
  ( check ( Nothing ^. non "default"
          , "default"
          )
  , check ( Nothing & non 100 +~ 7
          , Just 107
          )
  , check ( M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)]
              ^. at "Broccoli" . non False
          , False
          )
  , check ( M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)]
            & at "Wario's Woods" . non 0 +~ 999
          , M.fromList [ ("Breath of the wild",22000000) , ("Odyssey",9070000) , ("Wario's Woods",999) ]
          )
  , check ( ["Math", "Science", "Geography"] ^. pre (ix 7) . non "Unscheduled"
          , "Unscheduled"
          )
  , check ( [1, 2, 3, 4] ^.. traversed . pre (filtered even) . non (-1)
          , [-1, 2, -1, 4]
          )
  )
