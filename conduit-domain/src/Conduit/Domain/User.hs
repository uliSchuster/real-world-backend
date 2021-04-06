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
module Conduit.Domain.User
  ( UserBio(..)
  , User(..)
  , mkUser
  , UserIdentity(..)
  )
where

import qualified Text.Email.Validate           as Email
import qualified Text.URI                      as URI
import qualified Data.Password.Argon2          as PW

import qualified Conduit.Domain.Types          as DT
import qualified Conduit.Domain.Username       as DUN

import           RIO


-- TODO: Properly hide the UserBio implementation and provide a sanitizing 
-- constructor.
newtype UserBio = UserBio {getUserBio :: Text}
  deriving (Eq, Show, Ord, IsString, Semigroup, Monoid, Display, Hashable)

data User
  = User
      { userEmail :: !Email.EmailAddress,
        userName :: !DUN.Username,
        userImageUrl :: !(Maybe URI.URI),
        userBio :: !(Maybe UserBio)
      }
  deriving (Eq, Show)

-- | Construct a valid `User` from Text values.
mkUser
  :: Text -- ^ Email
  -> Text   -- ^ Username
  -> Maybe Text -- ^ Image URL
  -> Maybe Text   -- ^ User Bio
  -> Maybe User
mkUser email uName imageUrl bio =
  User
    <$> DT.mkEmail email
    <*> DUN.mkUsername uName
    <*> mapM URI.mkURI        imageUrl
    <*> mapM (Just . UserBio) bio

-- | A user identity combines the openly available information in a user 
-- profile with a salted password hash of this user. We rely on the Argon2
-- passowrd hasing algorithm. Password hashing must be performed at the usecase
-- level, as it requires a random salt.
data UserIdentity = UserIdentity
    { userPwdHash :: !(PW.PasswordHash PW.Argon2)
    , profile :: !User}
