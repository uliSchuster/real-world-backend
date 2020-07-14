{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  GeneralizedNewtypeDeriving - Derive typeclasses for newtypes
--             :  NoImplicitPrelude - Use RIO instead
--
-- Domain data types and business logic to describe a blog user.
module Domain.User
  ( User (..),
  )
where

import Domain.Username
import RIO
import qualified Text.Email.Validate as Email
import qualified Text.URI as URI

newtype UserBio = UserBio {getUserBio :: Text}
  deriving (Eq, Show, Ord, IsString, Semigroup, Monoid, Display, Hashable)

data User
  = User
      { userEmail :: Email.EmailAddress,
        userName :: UserName,
        userImageUrl :: URI.URI,
        userBio :: UserBio
      }
  deriving (Eq, Show)
