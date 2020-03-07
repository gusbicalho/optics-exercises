{-# LANGUAGE NoOverloadedStrings #-}
module Prisms where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Functor
import Data.List.Lens (prefixed)
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

data Threither a b c
  = A a
  | B b
  | C c
  deriving (Eq, Show)

makePrisms ''Threither

_A':: (Choice p, Applicative f)
   => p a2 (f a1) -> p (Threither a2 b c) (f (Threither a1 b c))
_A' = prism build match
  where
    build = A
    match (A a) = Right a
    match (B b) = Left (B b)
    match (C c) = Left (C c)

exPrisms2 =
  ( check ( Right 35 & _Right +~ 5
          , Right 40 :: Either () Int
          )
  , check ( [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] ^.. folded . _Just
          , ["Mind","Power","Soul","Time"]
          )
  , check ( [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] & traversed . _Just <>~ " Stone"
          , [ Just "Mind Stone" , Just "Power Stone" , Nothing , Just "Soul Stone" , Nothing , Just "Time Stone" ]
          )
  , check ( Left (Right True, "Eureka!") & _Left . _1 . _Right %~ not
          , Left @_ @() (Right @() False, "Eureka!")
          )
  , check ( _Cons # ("Do",["Re", "Mi"])
          , ["Do", "Re", "Mi"]
          )
  , check ( isn't (_Show @Int) "not an int"
          , True
          )
  )

exPrisms3 =
  ( let input = (Just 1, Nothing, Just 3)
        output = [1, 3]
    in check ( input ^.. each . _Just
             , output
             )
  , let input = ('x', "yz")
        output = "xzy"
    in check ( input & _2 %~ reverse & review _Cons
             , output
             )
  , let input = "do the hokey pokey"
        output = Left @_ @() (Just (Right @() "do the hokey pokey"))
    in check ( _Left . _Just . _Right # input
             , output
             )
  )

_Prefix :: T.Text -> Prism' T.Text T.Text
_Prefix prefix = prism' embed match
  where
    embed = (prefix <>)
    match = T.stripPrefix prefix

_Factor :: Int -> Prism' Int Int
_Factor d = prism' embed match
  where
    embed = (d *)
    match ((`divMod` d) -> (q, r))
      | r == 0 = Just q
      | otherwise = Nothing

toFizzBuzz :: Getting String Int String
toFizzBuzz = _Factor 3 . to (const "Fizz") <> _Factor 5 . to (const "Buzz")
            `failing` to show

runFizzBuzz = forM_ [1..20] $ putStrLn . view toFizzBuzz

_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where
    embed (x, xs) = x : xs
    match [] = Left []
    match (x : xs) = Right (x, xs)

_Cycles :: Eq a => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where
    embed xs = mconcat $ replicate n xs
    match s
      | let (q, r) = length s `divMod` n
            part = take q s
      , r == 0
      , s == (take (length s) . cycle $ part)
      = Just part
      | otherwise = Nothing

firstLaw :: Eq a => Prism' s a -> a -> Bool
firstLaw p value = preview p (review p value) == Just value

secondLaw :: (Eq s, Eq a) => Prism' s a -> s -> Bool
secondLaw p s = case preview p s of
  Nothing -> True
  Just a -> review p a == s

thirdLaw :: Eq s => APrism s s a b -> s -> Bool
thirdLaw p s =
  case matching p s of
    Right _ -> True
    Left t -> case matching p t of
      Right _ -> False
      Left s' -> s == s'


_Contains :: forall a. Ord a => a -> Prism' (S.Set a) (S.Set a)
_Contains e = prism' embed match
  where
    embed = S.insert e
    match s
      | e `elem` s = Just (S.delete e s)
      | otherwise = Nothing

-- _Contains breaks the first law
-- >>> λ> firstLaw (_Contains 2) (S.fromList [1,2,3])
-- >>> False

_Singleton :: forall a. Prism' [a] a
_Singleton = prism' embed match
  where
    match :: [a] -> Maybe a
    match [a] = Just a
    match _ = Nothing
    embed :: a -> [a]
    embed a = [a]


type Path = [String]
type Body = String
data Request
  = Post Path Body
  | Get Path
  | Delete Path
  deriving (Eq, Show)
makePrisms ''Request

path :: Lens' Request Path
path = lens getter setter
  where
    getter (Post p _) = p
    getter (Get p) = p
    getter (Delete p) = p
    setter (Post _ body) p = Post p body
    setter (Get _) p = Get p
    setter (Delete _) p = Delete p

_PathPrefix :: String -> Prism' Request Request
_PathPrefix pathPrefix = prism' embed match
  where
    _prefix :: Prism' [String] [String]
    _prefix = prefixed [pathPrefix]
    -- Add the prefix to the path
    embed :: Request -> Request
    embed req = req & path %~ review _prefix
    -- Check if the prefix matches the path
    match :: Request -> Maybe Request
    match req = req & path %%~ preview _prefix
