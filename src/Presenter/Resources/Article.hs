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
module Presenter.Resources.Article
  ( Article(..)
  , TR.toResource
  )
where

import qualified Data.Aeson                    as J
import qualified Data.Time                     as T
import qualified Presenter.Resources.Profile   as RP
import qualified Presenter.Resources.ToResource
                                               as TR
import qualified Domain.Article                as DA
import qualified Domain.Title                  as DT
import qualified Domain.Content                as DC
import qualified Domain.Tag                    as DT
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
      { slug :: Text,
        title :: Text,
        description :: Text,
        body :: Text,
        tagList :: [Text],
        createdAt :: T.UTCTime,
        updatedAt :: T.UTCTime,
        favorited :: Bool,
        favoritesCount :: Integer,
        author :: RP.Profile
      }
  deriving (Show, Eq, Generic, J.ToJSON)


instance TR.ToResource DA.Article Article where
  toResource :: DA.Article -> Article
  toResource da = Article
    { slug           = DT.getSlug . DT.mkSlug . DA.articleTitle $ da
    , title          = DT.getTitle . DA.articleTitle $ da
    , description    = DC.getDescription . DA.articleDescription $ da
    , body           = DC.getBody . DA.articleBody $ da
    , createdAt      = DA.articleCreatedAt da
    , updatedAt      = DA.articleModifiedAt da
    , author         = TR.toResource . DA.articleAuthor $ da
    , favorited      = False -- TODO: Include favories with user auth
    , favoritesCount = 0 -- TODO: Include favories with user auth
    , tagList        = DT.getTag <$> DA.articleTags da
    }
