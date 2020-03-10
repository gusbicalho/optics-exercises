{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Text.RawString.QQ (r)

example :: String
example =
  [r|
    {
      "people": [
        "dumb val",
        {"name": "chaves"},
        {"name": "waldo", "personalData": {"age": 47}}
      ]
    }
  |]

waldosAge :: AsValue t => Traversal' t Integer
waldosAge = key "people"
          . values
          . filteredBy (key "name" . _String . only "waldo")
          . key "personalData"
          . key "age"
          . _Integer


-- λ>  example ^? waldosAge
-- Just 47
-- λ> example & waldosAge *~ 2
-- "{\"people\":[\"dumb val\",{\"name\":\"chaves\"},{\"name\":\"waldo\",\"personalData\":{\"age\":94}}]}"
