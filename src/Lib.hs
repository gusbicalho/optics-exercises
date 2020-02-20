module Lib where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

triple :: (Int, Double, T.Text)
triple = (1 :: Int, 2.6 :: Double, "hue" :: T.Text)

nested :: ((Double, T.Text), Int)
nested = ((2.6 :: Double, "hue" :: T.Text), 1 :: Int)

stories :: [T.Text]
stories = ["This one time at band camp", "Nuff said.", "This is a short story"]

summaries :: Traversable f
          => Int -> f T.Text -> f T.Text
summaries n = over shortTexts summarize
  where
    shortTexts = traversed . filtered ((> n) . T.length)
    summarize = (<> "...") . T.take n


data Ship =
  Ship
  { _name :: T.Text
  , _numCrew :: Int
  }
  deriving (Eq, Show)

makeLenses ''Ship

someShip :: Ship
someShip = Ship "Black Pearl" 37

data Promotion a = Promotion { _item :: a , _discountPercentage :: Double } deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens _item setter
  where
    setter p i = p { _item = i}

data Preferences a = Preferences { _best :: a , _worst :: a } deriving (Show)

flipPair (a,b) = (b,a)

prefs :: Lens (Preferences a) (Preferences b) (a,a) (b,b)
prefs = lens getter setter
  where
    getter Preferences {_best, _worst} = (_best, _worst)
    setter p (b, w) = p {_best = b, _worst = w}

data Result e = Result { _lineNumber :: Int , _result :: Either e String }

result :: Lens (Result a) (Result b) (Either a String) (Either b String)
result = lens getter setter
  where
    getter = _result
    setter e r = e {_result = r}

data Blublu a b c = Blublu a b c

_1_2 :: Lens (a, b, c) (d, e, c) (a, b) (d, e)
_1_2 = lens getter setter
  where
    getter (a, b, _) = (a, b)
    setter (_, _, c) (d, e) = (d, e, c)
