{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  Arrows - Required by Opaleye
--             :  FlexibleInstances - Required by Opaleye
--             :  MultiParamTypeClasses - Required by Opaleye
--             :  TemplateHaskell - Lets Opaleye generate the mapping function
--             :  NoImplicitPrelude - Use RIO instead
--             :  GeneralizedNewtypeDeriving - Simplify newtype usage
--             :  OverloadedStrings - Use Text literals
--
-- Database interface for the "user" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.Users
  ( UserIdT (..),
    UserIdField,
    OptionalUserIdField,
    UserId,
    pUserId,
    UserT (..),
    User,
    findAllUsers,
    findUser,
  )
where

import Control.Arrow ((<<<), arr, returnA)
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import Opaleye ((.==))
import Persistence.DbConfig (schemaName)
import Persistence.PersistenceUtils
import RIO
import RIO.List as L

--------------------
-- Dedicated User ID
--------------------

newtype UserIdT a = UserId {getUserId :: a}
  deriving (Eq, Show, Display)

$(makeAdaptorAndInstance "pUserId" ''UserIdT)

type UserIdField = UserIdT (F OE.SqlInt8)

type OptionalUserIdField = UserIdT (Maybe (F OE.SqlInt8))

type UserId = UserIdT Int64

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "user" table. This type describes the "shape" of
-- a record, but not the type of individual record fields. This allows Opaleye
-- map Haskell types to PostgreSQL types by specializing the type parameters.
data UserT key username email bio imageUrl pwdHash salt
  = User
      { userKey :: key,
        userUsername :: username,
        userEmail :: email,
        userBio :: bio,
        userImageUrl :: imageUrl,
        userPwdHash :: pwdHash,
        userSalt :: salt
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "user" table. This record uses
-- native PostgreSQL types. Read- and write-records are different to allow
-- PostgreSQL DB to perform actions upon writing certain fields, like
-- auto-generate a key, or set the insertion date.
type UserW =
  UserT
    OptionalUserIdField -- autogenerated key
    (F OE.SqlText) -- username
    (F OE.SqlText) -- email
    (FNull OE.SqlText) -- optional bio
    (FNull OE.SqlText) -- optional image url
    (F OE.SqlText) -- password hash
    (F OE.SqlText) -- password salt

-- | Record that Opaleye reads from the "user" table. This record uses nativ
-- PostreSQL types.
type UserR =
  UserT
    UserIdField -- key
    (F OE.SqlText) -- username
    (F OE.SqlText) -- email
    (FNull OE.SqlText) -- bio
    (FNull OE.SqlText) -- image url
    (F OE.SqlText) -- password hash
    (F OE.SqlText) -- password salt

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type User =
  UserT
    UserId -- key
    Text -- username
    Text -- email
    (Maybe Text) -- bio
    (Maybe Text) -- image url
    Text -- pwdHash
    Text -- Salt

instance Display User where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pUser" ''UserT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
userTable :: OE.Table UserW UserR
userTable =
  OE.tableWithSchema
    schemaName
    "users"
    ( pUser
        User
          { userKey = pUserId (UserId (OE.tableField "id")),
            userUsername = OE.tableField "username",
            userEmail = OE.tableField "email",
            userBio = OE.tableField "bio",
            userImageUrl = OE.tableField "image_url",
            userPwdHash = OE.tableField "password_hash",
            userSalt = OE.tableField "salt"
          }
    )

-- TODO: Create lenses for easier field access if needed.

-- $(makeLensesWith abbreviatedFields ''UserT)

--------------------
-- Queries
--------------------
-- The queries below are written in the typesafe Opaleye query DSL. Opaleye
-- translates them into actual SQL statements that can be executed against the
-- DBMS. The query statements specified below are similar to prepared
-- statements; they need to be executed separately.
-- Queries return Opaleye PostgreSQL "Read" records.

-- | Retrieve all users.
allUsersQ :: OE.Select UserR
allUsersQ = OE.selectTable userTable

userByNameQ :: OE.SelectArr (F OE.SqlText) UserR
userByNameQ = proc uName -> do
  row <- allUsersQ -< ()
  OE.restrict -< userUsername row .== uName
  returnA -< row

--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access, given a
-- connection string. These functions use Opaleye primitive that perform the
-- mapping between Haskell records and Opaleye PostgreSQL records.

-- | Find all users stored in the DB and return them.
-- Naming convention: DB retrievals are called "find".
findAllUsers :: PGS.ConnectInfo -> IO [User]
findAllUsers connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn allUsersQ
  PGS.close conn
  return result

-- | Find the user with given user name.
findUser :: PGS.ConnectInfo -> Text -> IO (Maybe User)
findUser connInfo uName = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn (userByNameQ <<< arr (const $ OE.sqlStrictText uName))
  PGS.close conn
  return $ L.headMaybe result
