{-# LANGUAGE NoOverloadedStrings #-}
module IndexedOptics where

import Control.Lens
import Control.Lens.Indexed
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Foldable
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

exIndexedOptics1 =
  ( check ( M.fromList [("streamResponse", False), ("useSSL", True)] ^@.. itraversed
          , [("streamResponse",False),("useSSL",True)]
          )
  , check ( (M.fromList [('a', 1), ('b', 2)], M.fromList [('c', 3), ('d', 4)])
              ^@.. both . itraversed
          , [('a',1),('b',2),('c',3),('d',4)]
          )
  , check ( M.fromList [('a', (True, 1)), ('b', (False, 2))]
              ^@.. itraversed <. _1
          , [('a', True), ('b', False)]
          )
  , check ( [ M.fromList [("Tulips", 5), ("Roses", 3)] , M.fromList [("Goldfish", 11), ("Frogs", 8)] ]
              ^@.. itraversed <.> itraversed
          , [ ((0,"Roses"), 3) , ((0,"Tulips"), 5) , ((1,"Frogs"), 8) , ((1,"Goldfish"), 11) ]
          )
  , check ( [10, 20, 30] & itraversed %@~ (+)
          , [10,21,32]
          )
  , check ( itraverseOf itraversed (\i s -> Just (replicate i ' ' <> s)) ["one", "two", "three"]
          , Just ["one", " two", "  three"]
          )
  , check ( itraverseOf itraversed (\n s -> Just $ show n <> ": " <> s) ["Go shopping", "Eat lunch", "Take a nap"]
          , Just ["0: Go shopping", "1: Eat lunch", "2: Take a nap"]
          )
  )

exercises :: M.Map String (M.Map String Int)
exercises = M.fromList [ ("Monday"   , M.fromList [("pushups", 10), ("crunches", 20)])
                       , ("Wednesday", M.fromList [("pushups", 15), ("handstands", 3)])
                       , ("Friday"   , M.fromList [("crunches", 25), ("handstands", 5)])
                       ]

exIndexFilters1 =
-- Compute the total number of “crunches” you should do this week.
  ( exercises & sumOf (traversed . itraversed . index "crunches")
-- Compute the number of reps you need to do across all exercise types on Wednesday.
  , exercises & sumOf (itraversed . index "Wednesday" . traversed)
-- List out the number of pushups you need to do each day, you can use ix to help this time if you wish.
  , exercises ^@.. itraversed <. ix "pushups"
  )

board :: [[Char]]
board = ["XOO"
        ,".XO"
        ,"X.."]

exIndexFilters2 =
--  Generate a list of positions alongside their (row, column) coordinates.
  ( check ( board ^@.. itraversed <.> itraversed
          , [((0,0),'X'),((0,1),'O'),((0,2),'O'),((1,0),'.'),((1,1),'X'),((1,2),'O'),((2,0),'X'),((2,1),'.'),((2,2),'.')]
          )
-- Set the empty square at (1, 0) to an 'X'.
-- HINT:When using the custom composition operators you’ll often need to
-- introduce parenthesis to get the right precedence.
  , check ( board & (itraversed <.> itraversed) . index (1,0) .~ 'X'
          , ["XOO"
            ,"XXO"
            ,"X.."]
          )
-- Get the 2nd column as a list (e.g. "OX."). Try to do it using index instead of indices!
  , check ( board ^.. (traversed . itraversed) . index 1
          , "OX."
          )
-- Get the 3rd row as a list (e.g. "X.."). Try to do it using index instead of indices!
-- HINT: The precedence for this one can be tricky too.
  , check ( board ^.. (itraversed <. traversed) . index 2
          , "X.."
          )
  )

data Board a
  = Board
    a a a
    a a a
    a a a
  deriving (Eq, Show, Foldable)

data Position = I | II | III
  deriving (Show, Eq, Ord)

testBoard :: Board Char
testBoard
  = Board
    'X' 'O' 'X'
    '.' 'X' 'O'
    '.' 'O' 'X'

slotsTraversal :: IndexedTraversal (Position, Position) (Board a) (Board b) a b
slotsTraversal p (Board a1 b1 c1
                        a2 b2 c2
                        a3 b3 c3)
  = Board <$> indexed p (I , I) a1
          <*> indexed p (II , I) b1
          <*> indexed p (III, I) c1
          <*> indexed p (I , II) a2
          <*> indexed p (II , II) b2
          <*> indexed p (III, II) c2
          <*> indexed p (I , III) a3
          <*> indexed p (II , III) b3
          <*> indexed p (III, III) c3

showBoard :: Board Char -> String
showBoard b = foldMap (uncurry slotStr) (b ^@.. slotsTraversal)
  where
    slotStr (III, _) c = c : "\n"
    slotStr (_, _) c = [c]
