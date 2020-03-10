{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Set as S
import qualified Data.Set.Lens as S.L
import qualified Data.Text as T
import qualified Data.Text.Lens as T.L
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

pods :: T.Text
pods = [r|
{
  "kind": "List",
  "apiVersion": "v1",
  "items": [
    {
      "kind": "Pod",
      "apiVersion": "v1",
      "metadata": {
        "name": "redis-h315w",
        "creationTimestamp": "2019-03-23T19:42:21Z",
        "labels": {
          "name": "redis",
          "region": "usa"
        }
      },
      "spec": {
        "containers": [
          {
            "name": "redis",
            "image": "redis",
            "ports": [
              {
                "name": "redis",
                "hostPort": 27017,
                "containerPort": 27017,
                "protocol": "TCP"
              }
            ],
            "resources": {
              "requests": {
                "cpu": "100m"
              }
            }
          }
        ]
      }
    },
    {
      "kind": "Pod",
      "apiVersion": "v1",
      "metadata": {
        "name": "web-4c5bj",
        "creationTimestamp": "2019-02-24T20:23:56Z",
        "labels": {
          "name": "web",
          "region": "usa"
        }
      },
      "spec": {
        "containers": [
          {
            "name": "web",
            "image": "server",
            "ports": [
              {
                "name": "http-server",
                "containerPort": 3000,
                "protocol": "TCP"
              }
            ],
            "resources": {
              "requests": {
                "cpu": "100m"
              }
            }
          }
        ]
      }
    }
  ]
}
|]

-- 1. Your first task should be mostly straightforward: get the api version which was used to make the call.
exKubeApi1 = pods ^? key "apiVersion" . _String

-- 2. Next, count the number of all containers across all pods. You can assume that every element of “items” is a pod.
exKubeApi2 = pods & lengthOf (key "items" . values . key "spec" . key "containers" . values)

-- 3. Return the “name” (as Text) of all containers which have the same value for their “image” and “name” fields
exKubeApi3 = pods ^.. key "items"
                    . values
                    . key "spec"
                    . key "containers"
                    . values
                    . filtered (\o -> (o ^? key "name") == (o ^? key "image"))
                    . key "name"
                    . _String

-- 4. Collect a list of all “containerPort”s alongside their Pod’s “metadata > name”.
exKubeApi4 = pods ^@.. key "items"
                    . values
                    . reindexed (preview $ key "metadata" . key "name" . _String) selfIndex
                    <. key "spec"
                    . key "containers"
                    . values
                    . key "ports"
                    . values
                    . key "containerPort"

-- 5. Uppercase the label values inside each pod’s metadata
exKubeApi5 = pods & key "items"
                  . values
                  . key "metadata"
                  . key "labels"
                  . members
                  . _String
                  %~ T.toUpper

-- 6. Set a resource request of memory: "256M" for every container
exKubeApi6 = pods & key "items"
                  . values
                  . key "spec"
                  . key "containers"
                  . values
                  . key "resources"
                  . key "requests"
                  . _Object
                  . at "memory"
                  ?~ "256M"

-- 7. Get a Set of all metadata label keys used in the response
exKubeApi7 = pods & S.L.setOf ( key "items"
                              . values
                              . key "metadata"
                              . key "labels"
                              . members
                              . asIndex
                              )

-- 8. Set the hostPort to 8080 on any “port” descriptions where it is unset
exKubeApi8 = pods & key "items"
                  . values
                  . key "spec"
                  . key "containers"
                  . values
                  . key "ports"
                  . values
                  . _Object
                  . at "hostPort"
                  . filteredBy _Nothing
                  ?~ _Number # 8080

-- 9. Prepend the region to the name of each container.
exKubeApi9 = pods & key "items"
                  . values
                  . reindexed (preview $ key "metadata" . key "labels" . key "region" . _String) selfIndex
                  <. key "spec"
                  . key "containers"
                  . values
                  . key "name"
                  . _String
                  %@~ \region name ->
                        case region of
                          Nothing -> name
                          Just region' -> region' <> "-" <> name
