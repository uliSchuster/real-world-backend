{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
--             :  DeriveAnyClass - To include ToJSON in th deriving clause
--             :  InstanceSigs - Write signatures for class instance functions
--             :  MultiParamTypeClasses - The Resource class has two parameters
--
-- Article resource used in the Conduit ReST API
module Conduit.Presenter.Resources.Article
  ( Article(..)
  , TR.toResource
  )
where

import qualified Data.Aeson                    as J
import qualified Data.Time                     as T
import qualified Conduit.Presenter.Resources.Profile
                                               as RP
import qualified Conduit.Presenter.Resources.ToResource
                                               as TR
import qualified Conduit.Domain.API            as D
import           RIO

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
      { slug :: !Text,
        title :: !Text,
        description :: !Text,
        body :: !Text,
        tagList :: ![Text],
        createdAt :: !T.UTCTime,
        updatedAt :: !T.UTCTime,
        favorited :: !Bool,
        favoritesCount :: !Integer,
        author :: !RP.Profile
      }
  deriving (Show, Eq, Generic, J.ToJSON)

instance TR.ToResource D.Article Article where
  toResource :: D.Article -> Article
  toResource da = Article
    { slug           = D.getSlug . D.mkSlug . D.articleTitle $ da
    , title          = D.getTitle . D.articleTitle $ da
    , description    = D.getDescription . D.articleDescription $ da
    , body           = D.getBody . D.articleBody $ da
    , createdAt      = D.articleCreatedAt da
    , updatedAt      = D.articleModifiedAt da
    , author         = TR.toResource . D.articleAuthor $ da
    , favorited      = False -- TODO: Include favories with user auth
    , favoritesCount = 0 -- TODO: Include favories with user auth
    , tagList        = D.getTag <$> D.articleTags da
    }
