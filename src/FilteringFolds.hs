module FilteringFolds where

import Control.Lens
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Monoid
import qualified Data.List as L
import qualified Data.Ord as Ord
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

check :: Eq a => (a, a) -> Maybe (a, a)
check (actual, expected)
  | actual == expected = Nothing
  | otherwise = Just (actual, expected)

-- A data structure to represent a single card
data Card = Card { _name :: String , _aura :: Aura , _holo :: Bool , _moves :: [Move] } deriving (Show, Eq)
-- Each card has an aura-type
data Aura = Wet | Hot | Spark | Leafy deriving (Show, Eq)
-- Cards have attack moves
data Move = Move { _moveName :: String , _movePower :: Int } deriving (Show, Eq)
makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck = [ Card "Skwortul" Wet False [Move "Squirt" 20]
       , Card "Scorchander" Hot False [Move "Scorch" 20]
       , Card "Seedasaur" Leafy False [Move "Allergize" 20]
       , Card "Kapichu" Spark False [Move "Poke" 10, Move "Zap" 30]
       , Card "Elecdude" Spark False [Move "Asplode" 50]
       , Card "Garydose" Wet True [Move "Gary's move" 40]
       , Card "Moisteon" Wet False [Move "Soggy" 3]
       , Card "Grasseon" Leafy False [Move "Leaf Cut" 30]
       , Card "Spicyeon" Hot False [Move "Capsaicisize" 40]
       , Card "Sparkeon" Spark True [Move "Shock" 40, Move "Battery" 50]
       ]

countSparks :: Int
countSparks = lengthOf ( folded . aura . filtered (== Spark) ) deck

countPowerfulMoves :: Int
countPowerfulMoves = lengthOf ( folded . moves . folded . movePower . filtered (> 30) ) deck

cardsWithGreatMoves :: [String]
cardsWithGreatMoves = deck ^.. folded
                             . filtered (anyOf (moves . folded . movePower) (> 40))
                             . name

countSparkMoves :: Int
countSparkMoves = lengthOf ( folded
                           . filtered ((== Spark) . view aura)
                           . moves
                           . folded
                           )
                           deck

greatSparkMoves :: [String]
greatSparkMoves = deck ^.. folded
                         . filteredBy (aura . only Spark)
                         . moves
                         . folded
                         . filteredBy (movePower . filtered (> 30))
                         . moveName

holoCardWithMostMoves :: Maybe Card
holoCardWithMostMoves = maximumByOf
                          (folded . filteredBy holo)
                          (Ord.comparing (lengthOf moves))
                          deck

testFiltering =
  ( -- List all the cards whose name starts with 'S'
    check ( deck ^.. folded . filteredBy (name . (taking 1 folded) . only 'S') . name
          , [ "Skwortul"
            , "Scorchander"
            , "Seedasaur"
            , "Spicyeon"
            , "Sparkeon"
            ]
          )
    -- What’s the lowest attack power of all moves?
  , check ( minimumOf (folded . moves . folded . movePower) deck
          , Just 3
          )
    -- What’s the name of the first card which has more than one move?
  , check ( deck ^? folded . filtered ((> 1) . lengthOf (moves . folded)) . name
          , Just "Kapichu"
          )
    -- Are there any Hot cards with a move with more than 30 attack power?
  , check ( has ( folded
                . filteredBy (aura . only Hot)
                . filteredBy (moves . folded . filteredBy (movePower . filtered (> 30))))
                deck
          , True
          )
    -- List the names of all holographic cards with a Wet aura.
  , check ( deck ^.. folded . filtered (view holo) . filteredBy (aura . only Wet) . name
          , ["Garydose"]
          )
    --  What’s the sum of all attack power for all moves belonging to non-Leafy cards?
  , check ( sumOf ( folded
                  . filteredBy (aura . filtered (/= Leafy))
                  . moves
                  . folded
                  . movePower )
                  deck
          , 303
          )
  )
