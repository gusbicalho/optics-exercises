{-# LANGUAGE NoOverloadedStrings #-}
module Folds where

import Control.Lens
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Monoid
import qualified Data.Ord as Ord
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

check (actual, expected)
  | actual == expected = Nothing
  | otherwise = Just (actual, expected)

testSimpleFolds1 =
  ( check ( beastSizes ^.. folded
          , beastSizes
          )
  , check ( beastSizes ^.. folded . folded
          , ["Sirens", "Kraken", "Ogopogo"]
          )
  , check (  beastSizes ^.. folded . folded . folded
          , "SirensKrakenOgopogo"
          )
  , check ( beastSizes ^.. folded . _2
          , ["Sirens", "Kraken", "Ogopogo"]
          )
  , check ( toListOf (folded . folded) [[1 :: Int, 2, 3], [4, 5, 6]]
          , [1,2,3,4,5,6]
          )
  , check ( toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
          , "CaptainFirst Mate"
          )
  , check (  ("Hello", "It's me") ^.. both . folded
          , "HelloIt's me"
          )
  , check ( ("Why", "So", "Serious?") ^.. each
          , ["Why", "So", "Serious?"]
          )
  , check ( ("Why", "So", "Serious?") ^.. each . folded
          , "WhySoSerious?"
          )
  , check ( [ (T.pack "Why", T.pack "So", T.pack "Serious?")
            , (T.pack "This", T.pack "is", T.pack "SPARTA")]
            ^.. each . each . each
          , "WhySoSerious?ThisisSPARTA"
          )
  )

testSimpleFolds2 =
  ( toListOf ( (folded :: Fold [(Int, Char)] (Int, Char))
             . (_1     :: Fold (Int, Char)   Int))
             [(1, 'a'), (2, 'b'), (3, 'c')]
      :: [Int]
  , (toListOf :: Fold (Bool, S.Set [Char]) [Char]
              -> (Bool, S.Set [Char])
              -> [[Char]])
      ( (_2     :: Fold (Bool, S.Set [Char]) (S.Set [Char]))
      . (folded :: Fold (S.Set [Char])       [Char]))
      (False, S.fromList ["one", "two", "three"])
      :: [[Char]]
  , (toListOf :: Fold (M.Map [Char] [Char]) Char
              -> M.Map [Char] [Char]
              -> [Char])
      ( (folded :: Fold (M.Map [Char] [Char]) [Char])
      . (folded :: Fold [Char]                Char))
      (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
  )

testSimpleFolds3 =
  ( check ( [1 :: Int, 2, 3] ^.. folded
          , [1,2,3]
          )
  , check (  ("Light", "Dark") ^.. _1
          , ["Light"]
          )
  , check ( [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . each
          , ["Light","Dark","Happy","Sad"]
          )
  , check ( [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
          , ["Light","Happy"]
          )
  , check ( [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _2 . folded
          , "DarkSad"
          )
  , check ( ("Bond", "James", "Bond") ^.. each
          , ["Bond","James","Bond"]
          )
  )

testCustomFolds1 =
  ( check (["Yer", "a", "wizard", "Harry"] ^.. folded . folded
        , "YerawizardHarry"
        )
  , check ([[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
        , [1, 2, 4, 5]
        )
  , check ([[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
        , [[1,2], [4,5]]
        )
  , check (["bob", "otto", "hannah"] ^.. folded . to reverse
        , ["bob", "otto", "hannah"]
        )
  , check (("abc", "def") ^.. folding (\(a, b) -> [a, b]). to reverse . folded
        , "cbafed"
        )
  , check (("abc", "def") ^.. folding (\(a, b) -> [a, b]). folding reverse
        , "cbafed"
        )
  )

testCustomFolds2 =
  ( check ( [1..5] ^..
              folded . to (100*)
          , [100,200,300,400,500]
          )
  , check ( (1, 2) ^..
              both
          , [1, 2]
          )
  , check ( [(1, "one"), (2, "two")] ^..
              folded . folded
          , ["one", "two"]
          )
  , check ( (Just 1, Just 2, Just 3) ^..
              each . folded
          , [1, 2, 3]
          )
  , check ( [Left 1, Right 2, Left 3] ^..
              folded . folded
          , [2]
          )
  , check ( [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^..
              folded . folding (\(a,b) -> a <> b)
          , [1, 2, 3, 4, 5, 6, 7, 8]
          )
  , check ( [1, 2, 3, 4] ^..
              folded . to (\case x | odd x -> Left x
                                   | otherwise -> Right x)
          , [Left 1, Right 2, Left 3, Right 4]
          )
  , check ( [(1, (2, 3)), (4, (5, 6))] ^..
              folded . folding (\(a,(b, c)) -> [a, b, c])
          , [1, 2, 3, 4, 5, 6]
          )

  , check ( [(Just 1, Left "one"), (Nothing, Right 2)] ^..
              folded . folding (\(m, e) -> m ^.. folded <> e ^.. folded)
          , [1, 2]
          )
  , check ( [(1, "one"), (2, "two")] ^..
              folded . folding (\(n, s) -> [Left n, Right s])
          , [Left 1, Right "one", Left 2, Right "two"]
          )
  , check ( S.fromList ["apricots", "apples"] ^..
              folded . folding reverse
          , "selppastocirpa"
          )
  , check ( [(12, 45, 66), (91, 123, 87)] ^..
              folded . _2 . to show . folding reverse
          , "54321"
          )
  , check ( [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^..
              folded . folding (\case (n, s) | even n -> [s]
                                             | otherwise -> [])
          , ["b", "d"]
          )
  )

data Actor = Actor { _name :: String
                   , _birthYear :: Int
                   }
  deriving (Show, Eq)
data TVShow = TVShow { _title :: String
                     , _numEpisodes :: Int
                     , _numSeasons :: Int
                     , _criticScore :: Double
                     , _actors :: [Actor]
                     }
  deriving (Show, Eq)

makeLenses ''Actor
makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother = TVShow { _title = "How I Met Your Mother"
                           , _numEpisodes = 208
                           , _numSeasons = 9
                           , _criticScore = 83
                           , _actors = [ Actor "Josh Radnor" 1974
                                       , Actor "Cobie Smulders" 1982
                                       , Actor "Neil Patrick Harris" 1973
                                       , Actor "Alyson Hannigan" 1974
                                       , Actor "Jason Segel" 1980
                                       ]
                           }

buffy :: TVShow
buffy = TVShow { _title = "Buffy the Vampire Slayer"
               , _numEpisodes = 144
               , _numSeasons = 7
               , _criticScore = 81
               , _actors = [ Actor "Sarah Michelle Gellar" 1977
                           , Actor "Alyson Hannigan" 1974
                           , Actor "Nicholas Brendon" 1971
                           , Actor "David Boreanaz" 1969
                           , Actor "Anthony Head" 1954
                           ]
               }

tvShows :: [TVShow]
tvShows = [ howIMetYourMother
          , buffy
          ]

comparingOf :: Ord a
            => Getting a s a
            -> s -> s -> Ordering
comparingOf l = Ord.comparing (view l)

totalEpisodes = sumOf (folded . numEpisodes) tvShows

bestCriticScore = maximumOf (folded . criticScore) tvShows

oldestActor = minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows

age :: Int -> Getting Int Actor Int
age refYear = birthYear . to (refYear -)

showActor refYear a = a ^. name <> ": " <> (show $ a ^. age refYear)

ageSummary :: Int -> Actor -> (Sum Int, Sum Int)
ageSummary refYear actor = (Sum 1, Sum (actor ^. age refYear))

computeAverage :: (Sum Int, Sum Int) -> Double
computeAverage (Sum count, Sum total) = fromIntegral total / fromIntegral count

countShowsByActor :: M.Map String Integer
countShowsByActor = foldMapByOf
                      ( folded
                      . actors
                      . folded
                      . name )
                      (M.unionWith (+))
                      mempty
                      (\n -> M.singleton n 1)
                      tvShows

testFoldActions1 =
  ( check ( has folded []
          , False
          )
  , check ( foldOf both ("Yo", "Adrian!")
          , "YoAdrian!"
          )
  , check ( elemOf each "phone" ("E.T.", "phone", "home")
          , True
          )
  , check ( minimumOf folded [5, 7, 2, 3, 13, 17, 11]
          , Just 2
          )
  , check ( lastOf folded [5, 7, 2, 3, 13, 17, 11]
          , Just 11
          )
  , check ( anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
          , True
          )
  , check ( findOf folded even [11, 22, 3, 5, 6]
          , Just 22
          )
  )

testFoldActions2 =
  ( check ( findOf folded (\s -> s == reverse s) ["umbrella", "olives", "racecar", "hammer"]
          , Just "racecar"
          )
  , check ( allOf each even (2, 4, 6)
          , True
          )
  , check ( maximumByOf folded (comparingOf _1) [(2, "I'll"), (3, "Be"), (1, "Back")]
          , Just (3, "Be")
          )
  , check ( getSum $ foldMapOf both Sum (1 :: Int, 2 :: Int)
          , 3 :: Int
          )
  )

vowelsCount = length . filter (`elem` ['a','e','i','o','u'])

testFoldActions3 =
  ( check ( maximumByOf (folding words) (Ord.comparing vowelsCount) "Do or do not, there is no try."
          , Just "there"
          )
  , check ( foldOf (to reverse . folded) ["a", "b", "c"]
          , "cba"
          )
  , check ( foldOf (folded . _2 . to show . to reverse) [(12, 45, 66), (91, 123, 87)]
          , "54321"
          )
  , check ( foldOf (folded . to (\case (n, s) | even n -> [s]
                                              | otherwise -> []))
                   [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
          , ["b", "d"]
          )
  )

testHigherOrderFolds1 =
  ( check ( "Here's looking at you, kid" ^.. dropping 7 folded
          , "looking at you, kid"
          )
  , check ( ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (folding words)
          , ["My","Hakuna","No"]
          )
  , check ( ["My Precious", "Hakuna Matata", "No problemo"] ^.. taking 1 (folded . folding words)
          , ["My"]
          )
  , check ( ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (folding words) . folded
          , "MyHakunaNo"
          )
  , check ( sumOf (taking 2 each) (10 :: Integer, 50, 100)
          , 60
          )
  , check ( ("stressed", "guns", "evil") ^.. backwards each
          , ["evil","guns","stressed"]
          )
  , check ( ("stressed", "guns", "evil") ^.. backwards each . to reverse
          , ["live","snug","desserts"]
          )
  , check ( "blink182 k9 blazeit420" ^.. folding words . droppingWhile isAlpha folded
          , "1829420"
          )
  )

higherOrderFolds2Sample :: [Int]
higherOrderFolds2Sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile pred = backwards . droppingWhile pred . backwards . droppingWhile pred

testHigherOrderFolds2 =
  ( check ( lengthOf (takingWhile (<= 0) folded) higherOrderFolds2Sample
          , 2
          )
  , check ( maximumOf (taking 4 folded) higherOrderFolds2Sample
          , Just 4
          )
  , check ( higherOrderFolds2Sample ^? (dropping 1 . droppingWhile (/= 4) $ folded)
          , Just 3
          )
  , check ( lengthOf (takingWhile (< 0) (backwards folded)) higherOrderFolds2Sample
          , 2
          )
  , check ( higherOrderFolds2Sample ^.. (takingWhile (> 0) . droppingWhile (< 0) $ folded)
          , [4,3,8,6]
          )
  , check ( higherOrderFolds2Sample ^.. (trimmingWhile (< 0) folded)
          , [4, 3, 8, 6, -2, 3]
          )
  , check ( True
          , True
          )
  )
