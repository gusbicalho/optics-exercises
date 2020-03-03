{-# LANGUAGE NoOverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Traversals
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-----------------------------------------------------------------------------
module Traversals where

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

foo :: [(Bool, Integer)]
foo = [(True, 17), (False, 10), (False, 9), (True, 5)]
        & each . filtered fst . _2 %~ (* 100)
        & traversed . filtered (not . fst) . _2 %~ (* 5)

smash :: [(Integer, (Integer, Integer))]
smash = [(1,(2,3)), (4,(5,6))]
          & traversed . beside id both %~ (* 2)

splash :: [Either [Integer] Integer]
splash = [Right 7, Left [1,2,3], Right 1]
           & traversed . beside traversed id %~ (* 10)

exSimpleTraversals2 =
  ( check ( ("Jurassic", "Park")
              & both .~ "N/A"
          , ("N/A", "N/A")
          )
  , check ( ("Jurassic", "Park")
              & both . traversed .~ 'x'
          , ("xxxxxxxx", "xxxx")
          )
  , check ( ("Malcolm", ["Kaylee", "Inara", "Jayne"])
              & beside id traversed %~ take 3
          , ("Mal", ["Kay", "Ina", "Jay"])
          )
  , check ( ("Malcolm", ["Kaylee", "Inara", "Jayne"])
              & _2 . element 1 .~ "River"
          , ("Malcolm", ["Kaylee", "River", "Jayne"])
          )
  , check ( ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
              & traversed . elementOf worded 1 . traversed .~ 'x'
          , [ "Die xxxxxxx Day" , "Live xxx Let Die" , "You xxxx Live Twice" ]
          )

  , check ( ((1, 2), (3, 4))
              & both . both +~ 1
          , ((2, 3), (4, 5))
          )
  , check ( (1, (2, [3, 4]))
              & beside id (beside id traversed) +~ 1
          , (2, (3, [4, 5]))
          )
  , check ( ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
              & each . filtered fst . _2 . taking 5 traversed %~ toUpper
          , ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))
          )
  , check ( ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
              & each %~ snd
          , ("Strawberries", "Blueberries", "Blackberries")
          )



  , check ( True
          , True)
  )

exTraversalActions1 =
  ( check ( sequenceAOf _1 (Nothing :: Maybe Int, "Rosebud")
          , Nothing
          )
  , check ( sequenceAOf (traversed . _1) [("ab", 1), ("cd", 2)]
          , [ [('a', 1) ,('c', 2)]
            , [('a', 1) ,('d', 2)]
            , [('b', 1) ,('c', 2)]
            , [('b', 1) ,('d', 2)]]
          )
  , check ( sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]]
          ,  ZipList {getZipList = [[1,3],[2,4]]}
          )
  , check ( sequenceAOf (traversed . _2) [('a', ZipList [1,2]), ('b', ZipList [3,4])]
          ,  ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}
          )
  , check ( let result = traverseOf (beside traversed both) (\n -> modify (+n) >> get) ([1, 1, 1], (1, 1))
            in runState result 0
          , (([1,2,3],(4,5)), 5)
          )

  , check ( True
          , True)
  )

exTraversalActions2 =
  ( check ( ("ab", True)
              & _1 . traversed
              %%~ \c -> [toLower c, toUpper c]
          , traverseOf (_1 . traversed) (\c -> [toLower c, toUpper c]) ("ab", True)
          )
  , check ( [('a', True), ('b', False)]
              & traversed . _1
              %%~ \c -> [toLower c, toUpper c]
          , traverseOf (traversed . _1) (\c -> [toLower c, toUpper c]) [('a', True), ('b', False)]
          )
  )

data User = User { _name :: String , _age :: Int } deriving (Eq, Show)
makeLenses ''User
data Account = Account { _id :: String , _user :: User } deriving (Eq, Show)
makeLenses ''Account
validateAge :: Account -> Either String Account
validateAge = user . age %%~ \case age' | 0 < age' && age' < 150 -> Right age'
                                        | otherwise -> Left ("Invalid age: " <> show age')

exTraversalActions3 =
  ( check ( Account "someid" (User "uname" 149) & validateAge
          , Right (Account "someid" (User "uname" 149))
          )
  , check ( Account "someid" (User "uname" 150) & validateAge
          , Left "Invalid age: 150"
          )
  )

-- Values :: Traversal [a] [b] a b
values :: Applicative f => (a -> f b) -> [a] -> f [b]
values _ [] = pure []
values f (a:as) = (:) <$> f a <*> values f as


data Transaction = Withdrawal {_amount :: Int} | Deposit {_amount :: Int}
  deriving (Eq, Show)
makeLenses ''Transaction

newtype BankAccount = BankAccount { _transactions :: [Transaction] }
  deriving (Eq, Show)
makeLenses ''BankAccount

effectiveAmount :: Lens' Transaction Int
effectiveAmount = lens getter setter
  where
    getter (Deposit a) = a
    getter (Withdrawal a) = negate a
    setter _ a
      | a < 0 = Withdrawal (negate a)
      | otherwise = Deposit a

balance = sumOf (transactions . traversed . effectiveAmount)

deposits :: Applicative f => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits _ [] = pure []
deposits handler (w@(Withdrawal _) : ts)
  = liftA2 (:) (pure w) (deposits handler ts)
deposits handler (Deposit a : ts)
  = liftA2 (:) (Deposit <$> handler a) (deposits handler ts)

aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]

myAmountT :: Traversal' Transaction Int
myAmountT handler (Deposit a)    = Deposit <$> handler a
myAmountT handler (Withdrawal a) = Withdrawal <$> handler a

myBoth :: Traversal (a, a) (b, b) a b
myBoth handler (a1, a2) = liftA2 (,) (handler a1) (handler a2)

myLeft :: Traversal (Either a b) (Either a' b) a a'
myLeft _ (Right b) = Right <$> pure b
myLeft handler (Left a) = Left <$> handler a

myBeside :: Traversal s t a b
         -> Traversal s' t' a b
         -> Traversal (s,s') (t,t') a b
myBeside travA travB handler (a, b) = liftA2 (,) (a & travA %%~ handler)
                                                 (b & travB %%~ handler)

exPartsOf =
  (
  -- Viewing
    check ( [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
          , [2, 4]
          )
  , check ( ["Aardvark", "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed)
          , "AarBanCap"
          )
  , check ( ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf (beside traversed traversed)
          , [1,2,3,4]
          )
  -- Setting
  , check ( [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40]
          , [1,20,3,40]
          )
  , check ( ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Kangaroo"
          , ["Kangaroo","Bandicoot","Capybara"]
          )
  , check ( ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Ant"
          , ["Antdvark", "Bandicoot", "Capybara"]
          )
  -- Modifying -- Tip: Map values are traversed in order by KEY
  , check ( M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed %~ (\xs -> drop 1 xs <> take 1 xs)
          , M.fromList [('a','b'),('b','c'),('c','a')]
          )
  , check ( ('a', 'b', 'c') & partsOf each %~ reverse
          , ('c', 'b', 'a')
          )
  , check ( [1, 2, 3, 4, 5, 6] & partsOf (taking 3 traversed) %~ reverse
          , [3,2,1,4,5,6]
          )
  , check ( ('a', 'b', 'c') & unsafePartsOf each %~ \xs -> fmap ((,) xs) xs
          , (("abc",'a'),("abc",'b'),("abc",'c'))
          )
  )
