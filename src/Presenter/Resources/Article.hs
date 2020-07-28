{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--             :  DeriveGeneric - To automatically derive JSON mappers
--
-- Article resource used in the Conduit ReST API
module Presenter.Resources.Article
  ( Article (..),
  )
where

import qualified Data.Aeson as J
import qualified Data.Time as DT
import qualified Presenter.Resources.Profile as RP
import RIO

-- {
--   "article": {
--     "slug": "how-to-train-your-dragon",
--     "title": "How to train your dragon",
--     "description": "Ever wonder how?",
--     "body": "It takes a Jacobian",
--     "tagList": ["dragons", "training"],
--     "createdAt": "2016-02-18T03:22:56.637Z",
--     "updatedAt": "2016-02-18T03:48:35.824Z",
--     "favorited": false,
--     "favoritesCount": 0,
--     "author": {
--       "username": "jake",
--       "bio": "I work at statefarm",
--       "image": "https://i.stack.imgur.com/xHWG8.jpg",
--       "following": false
--     }
--   }
-- }
data Article
  = Article
      { slug :: Text,
        title :: Text,
        description :: Text,
        body :: Text,
        tagList :: [Text],
        createdAt :: DT.UTCTime,
        updatedAt :: DT.UTCTime,
        favorited :: Bool,
        favoritesCount :: Integer,
        author :: RP.Profile
      }
  deriving (Show, Eq, Generic)

instance J.ToJSON Article
