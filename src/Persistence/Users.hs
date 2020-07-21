{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Persistence.Users where

-- import qualified Control.Arrow ()
-- import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
-- import qualified Database.PostgreSQL.Simple ()
import qualified Opaleye as OE
import RIO

data UserPoly key username email bio imageUrl
  = User
      { userKey :: key,
        userUsername :: username,
        userEmail :: email,
        userBio :: bio,
        userImageUrl :: imageUrl
      }
  deriving (Show)

type UserPGWrite =
  UserPoly
    (Maybe (OE.Column OE.PGInt8)) -- key
    (OE.Column OE.PGText) -- username
    (OE.Column OE.PGText) -- email
    (OE.Column (OE.Nullable OE.PGText)) -- bio
    (OE.Column (OE.Nullable OE.PGText)) -- image url

type UserPGRead =
  UserPoly
    (OE.Column OE.PGInt8) -- key
    (OE.Column OE.PGText) -- username
    (OE.Column OE.PGText) -- email
    (OE.Column (OE.Nullable OE.PGText)) -- bio
    (OE.Column (OE.Nullable OE.PGText)) -- image url

type User =
  UserPoly
    Integer -- key
    Text -- username
    Text -- email
    (Maybe Text) -- bio
    (Maybe Text) -- image url

$(makeAdaptorAndInstance "pUser" ''UserPoly)

-- $(makeLensesWith abbreviatedFields ''UserPoly)

userTable :: OE.Table UserPGWrite UserPGRead
userTable =
  OE.Table
    "users"
    ( pUser
        User
          { userKey = OE.optional "id",
            userUsername = OE.required "username",
            userEmail = OE.required "email",
            userBio = OE.required "bio",
            userImageUrl = OE.required "image_url"
          }
    )
