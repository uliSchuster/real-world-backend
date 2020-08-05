{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  Arrows - For Opaleye queries
--             :  FlexibleInstances - For Opaleye table types
--             :  NoImplicitPrelude - Use RIO instead
--             :  OverloadedStrings - Use Text literals
--
-- Abstract collection-like interface to the underlying persistence layer.
module Conduit.Persistence.UserRepository
  ( readUser
  , findAllUsers
  )
where

import           Control.Arrow                  ( (<<<)
                                                , arr
                                                , returnA
                                                )
import qualified Database.PostgreSQL.Simple    as PGS
import qualified Opaleye                       as OE
import           Opaleye                        ( (.===) )
import qualified Conduit.Domain.User           as DU
import qualified Conduit.Domain.Username       as DUN
import qualified Conduit.Persistence.DbConfig  as DBC
import qualified Conduit.Persistence.UsersTable
                                               as PU
import           Conduit.Persistence.PersistenceUtils
import           RIO
import           RIO.List                      as L


-- | Returns a specific user stored in the underlying persistence mechanism
-- Naming convention: Read repository operations are called "read".
readUser
  :: (DBC.HasDbConnInfo cfg) => DUN.Username -> RIO cfg (Either Text DU.User)
readUser (DUN.Username uName) = do
  connInfo <- view DBC.connInfoL
  pUser    <- liftIO $ findUser connInfo uName
  case pUser of
    Nothing -> return $ Left ("Could not find user with username " <> uName)
    Just u  -> return $ toUser u

-- | Helper function that converts the data obtained from the DB into a
-- corresponding domain object. Because the domain types are shielded through
-- smart constructors, we need to handle the case here that a user read from
-- the DB does not conform to the domain restrictions. For now, we do it in the
-- simplest possible way: return an error message.
toUser :: PU.User -> Either Text DU.User
toUser pu = case maybeUser of
  Just user -> Right user
  Nothing ->
    Left
      $  "The user "
      <> PU.userUsername pu
      <> " stored with database ID "
      <> tshow (PU.userKey pu)
      <> " is invalid."
 where
  maybeUser = DU.mkUser (PU.userEmail pu)
                        (PU.userUsername pu)
                        (PU.userImageUrl pu)
                        (PU.userBio pu)

--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access, given a
-- connection string. These functions use Opaleye primitives that perform the
-- mapping between Haskell records and Opaleye PostgreSQL records.

-- | Find all users stored in the DB and return them.
-- Naming convention: DB retrievals are called "find".
findAllUsers :: PGS.ConnectInfo -> IO [PU.User]
findAllUsers connInfo = do
  conn   <- PGS.connect connInfo
  result <- OE.runSelect conn PU.allUsersQ
  PGS.close conn
  return result

-- | Find the user with given user name.
findUser :: PGS.ConnectInfo -> Text -> IO (Maybe PU.User)
findUser connInfo uName = do
  conn   <- PGS.connect connInfo
  result <- OE.runSelect
    conn
    (userByNameQ <<< arr (const $ OE.sqlStrictText uName)) -- Convert a parameter into an arrow via the const function.
  PGS.close conn
  return $ L.headMaybe result

--------------------
-- Compund Queries
--------------------
-- The queries below are written in the typesafe Opaleye query DSL. Opaleye
-- translates them into actual SQL statements that can be executed against the
-- DBMS. The query statements specified below are similar to prepared
-- statements; they need to be executed separately.
-- Queries return Opaleye PostgreSQL "Read" records.

userByNameQ :: OE.SelectArr (F OE.SqlText) PU.UserR
userByNameQ = proc uName -> do
  row <- PU.allUsersQ -< ()
  OE.restrict -< PU.userUsername row .=== uName
  returnA -< row