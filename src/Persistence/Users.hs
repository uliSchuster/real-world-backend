{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Persistence.Users
  ( UserT,
    User,
    getAllUsers,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import RIO

-- Type synonyms for convenience
type F field = OE.Field field

type FNull field = OE.FieldNullable field

data UserT key username email bio imageUrl
  = User
      { userKey :: key,
        userUsername :: username,
        userEmail :: email,
        userBio :: bio,
        userImageUrl :: imageUrl
      }
  deriving (Show)

type UserW =
  UserT
    (Maybe (F OE.SqlInt8)) -- key
    (F OE.SqlText) -- username
    (F OE.SqlText) -- email
    (FNull OE.SqlText) -- bio
    (FNull OE.SqlText) -- image url

type UserR =
  UserT
    (F OE.SqlInt8) -- key
    (F OE.SqlText) -- username
    (F OE.SqlText) -- email
    (FNull OE.SqlText) -- bio
    (FNull OE.SqlText) -- image url

type User =
  UserT
    Int64 -- key
    Text -- username
    Text -- email
    (Maybe Text) -- bio
    (Maybe Text) -- image url

$(makeAdaptorAndInstance "pUser" ''UserT)

-- $(makeLensesWith abbreviatedFields ''UserT)

userTable :: OE.Table UserW UserR
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

-- | Retrieve all users.
selectUsers :: OE.Select UserR
selectUsers = OE.selectTable userTable

getAllUsers :: PGS.Connection -> IO [User]
getAllUsers conn = OE.runSelect conn selectUsers
