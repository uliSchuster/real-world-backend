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
--
-- Database interface for the "follows" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.Follows
  ( UserId (..),
    Follows,
    getAllFollows,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import Persistence.Users (UserId (..))
import Persistence.PersistenceUtils
import RIO

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "follows" table.
data FollowsT key
  = Follows
      { followerFk :: key,
        followeeFk :: key
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "follows" table.
type FollowsW = FollowsT (F OE.SqlInt8) -- user FK

-- | Record that Opaleye reads from the "follows" table.
type FollowsR = FollowsT (F OE.SqlInt8) -- user FK

-- | Typesafe Haskell record to interface with the application.
type Follows = FollowsT UserId -- user fk

instance Display Follows where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pFollows" ''FollowsT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
followsTable :: OE.Table FollowsW FollowsR
followsTable =
  OE.Table
    "follows"
    ( pFollows
        Follows
          { followerFk = OE.required "follower_fk",
            followeeFk = OE.required "followee_fk"
          }
    )

--------------------
-- Queries
--------------------
-- The queries below are written in the typesafe Opaleye query DSL. Opaleye
-- translates them into actual SQL statements that can be executed against the
-- DBMS. The query statements specified below are similar to prepared
-- statements; they need to be executed separately.
-- Queries return Opaleye PostgreSQL "Read" records.

-- | Retrieve all follow relations.
selectFollows :: OE.Select FollowsR
selectFollows = OE.selectTable followsTable

--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access, given a
-- connection string. These functions use Opaleye primitive that perform the
-- mapping between Haskell records and Opaleye PostgreSQL records.

getAllFollows :: PGS.ConnectInfo -> IO [Follows]
getAllFollows connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn selectFollows
  PGS.close conn
  return result
