{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Conduit use cases for displaying and following users.
-- Uses an abstract repository to access the persisted user profiles.
module Usecases.ProfileUsecases
  ( getProfile
  )
where

import qualified Domain.User                   as DU
import qualified Domain.Username               as DUN
import           RIO
import           Usecases.UserRepositoryI

-- | Use the tag-repository configured at application-level to read all tags.
-- For those tags that fail to map on the domain type, log an error.
getProfile
  :: UserRepositoryI cfg => DUN.Username -> RIO cfg (Either Text DU.User)
getProfile = readUser
