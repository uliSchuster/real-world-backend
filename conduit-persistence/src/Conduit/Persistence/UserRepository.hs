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
  , insertUserIdentities
  )
where

import           Control.Arrow                  ( (<<<)
                                                , arr
                                                , returnA
                                                )
import qualified Opaleye                       as OE
import           Opaleye                        ( (.===) )
import qualified Conduit.Domain.API            as D
import qualified Conduit.Persistence.DbConfig  as DBC
import qualified Conduit.Persistence.UsersTable
                                               as PU
import           Conduit.Persistence.PersistenceUtils
import           RIO
import qualified RIO.List                      as L
import qualified Data.Password.Argon2          as PW


-- | Returns a specific user stored in the underlying persistence mechanism
-- Naming convention: Read repository operations are called "read".
readUser
  :: (DBC.HasDbConnPool cfg) => D.Username -> RIO cfg (Either Text D.User)
readUser (D.Username uName) = do
  connPool <- view DBC.connPoolL
  pUser    <- liftIO $ findUser connPool uName
  case pUser of
    Nothing -> return $ Left ("Could not find user with username " <> uName)
    Just u  -> return $ toUser u

-- | Helper function that converts the data obtained from the DB into a
-- corresponding domain object. Because the domain types are shielded through
-- smart constructors, we need to handle the case here that a user read from
-- the DB does not conform to the domain restrictions. For now, we do it in the
-- simplest possible way: return an error message.
toUser :: PU.User -> Either Text D.User
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
  maybeUser = D.mkUser (PU.userEmail pu)
                       (PU.userUsername pu)
                       (PU.userImageUrl pu)
                       (PU.userBio pu)

insertUserIdentities
  :: (DBC.HasDbConnPool cfg)
  => [D.UserIdentity]
  -> RIO cfg (Either Text [PU.UserId])
insertUserIdentities userIdentities = do
  connPool  <- view DBC.connPoolL
  userIds <- liftIO $ insertUsers connPool insertSpec
  return $ Right userIds
 where
  newUsers   = toPersistenceEntity <$> userIdentities
  insertSpec = OE.Insert
    { OE.iTable      = PU.userTable
    , OE.iRows       = map OE.toFields newUsers
    , OE.iReturning  = OE.rReturning (\PU.User { PU.userKey = pk } -> pk)
    , OE.iOnConflict = Nothing
    }

toPersistenceEntity :: D.UserIdentity -> PU.UserI
toPersistenceEntity u = PU.User
  { PU.userKey      = PU.UserId (Nothing :: Maybe Int64) -- DBMS generates key.
  , PU.userUsername = D.getUsername $ D.userName user
  , PU.userEmail    = D.emailToText $ D.userEmail user
  , PU.userBio      = D.getUserBio <$> D.userBio user
  , PU.userImageUrl = D.uriToText <$> D.userImageUrl user
  , PU.userPwdHash  = PW.unPasswordHash $ D.userPwdHash u
  }
  where user = D.profile u


--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access, given a
-- connection string. These functions use Opaleye primitives that perform the
-- mapping between Haskell records and Opaleye PostgreSQL records.

-- | Find all users stored in the DB and return them.
-- Naming convention: DB retrievals are called "find".
findAllUsers :: DBC.ConnPool -> IO [PU.User]
findAllUsers connPool =
  withPostgreSQLPool connPool $ \conn -> OE.runSelect conn PU.allUsersQ

-- | Find the user with given user name.
findUser :: DBC.ConnPool -> Text -> IO (Maybe PU.User)
findUser connPool uName = withPostgreSQLPool connPool $ \conn -> do
  result <- OE.runSelect
    conn
    (userByNameQ <<< arr (const $ OE.sqlStrictText uName)) -- Convert a parameter into an arrow via the const function.
  return $ L.headMaybe result

insertUsers :: DBC.ConnPool -> OE.Insert [PU.UserId] -> IO [PU.UserId]
insertUsers connPool insertSpec =
  withPostgreSQLPool connPool $ \conn -> OE.runInsert_ conn insertSpec


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