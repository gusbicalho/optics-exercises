module Nested where

import Control.Lens
import Control.Applicative
import Data.Char
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Person
  = Person
    { _name :: T.Text
    , _address :: Address
    }
  deriving (Show)

data Address
  = Address
    { _streetAddress :: StreetAddress
    , _city :: T.Text
    , _country :: T.Text
    }
  deriving (Show)

data StreetAddress
  = StreetAddress
    { _streetNumber :: T.Text
    , _streetName :: T.Text
    }
  deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock :: Person
sherlock
  = Person
    { _name = "S. Holmes"
    , _address
        = Address
          { _streetAddress
              = StreetAddress
                { _streetNumber = "221B"
                , _streetName = "Baker Street"
                }
          , _city = "London"
          , _country = "England"
          }
    }


data Armadillo = Armadillo deriving (Eq, Show)
data Hedgehog = Hedgehog deriving (Eq, Show)
data Platypus = Platypus Armadillo deriving (Eq, Show)
data BabySloth = BabySloth Hedgehog deriving (Eq, Show)
madScience :: Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
madScience a2h = \(Platypus a) -> BabySloth <$> a2h a
