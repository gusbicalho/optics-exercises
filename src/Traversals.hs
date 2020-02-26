module Traversals where

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

foo :: [(Bool, Integer)]
foo = [(True, 17), (False, 10), (False, 9), (True, 5)]
        & each . filtered fst . _2 %~ (* 100)
        & traversed . filtered (not . fst) . _2 %~ (* 5)

smash = [(1,(2,3)), (4,(5,6))]
          & traversed . beside id both %~ (* 2)

splash = [Right 7, Left [1,2,3], Right 1]
           & traversed . beside traversed id %~ (* 10)
