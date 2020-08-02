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
-- Profile resource used in the Conduit ReST API
module Presenter.Resources.Profile
  ( Profile (..),
    TR.toResource,
  )
where

import qualified Data.Aeson as J
import qualified Domain.User as DU
import qualified Domain.Username as DUN
import qualified Presenter.Resources.ToResource as TR
import RIO
import qualified Text.URI as URI

-- {
--   "profile": {
--     "username": "jake",
--     "bio": "I work at statefarm",
--     "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
--     "following": false
--   }
-- }
data Profile
  = Profile
      { username :: Text,
        bio :: Maybe Text,
        image :: Maybe Text,
        following :: Bool
      }
  deriving (Show, Eq, Generic, J.ToJSON)

instance TR.ToResource DU.User Profile where
  toResource :: DU.User -> Profile
  toResource du =
    Profile
      { username = DUN.getUsername . DU.userName $ du,
        bio = DU.getUserBio <$> DU.userBio du,
        image = URI.render <$> DU.userImageUrl du,
        following = False -- TODO: Map following status once login is available.
      }
