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
-- Lang. Ext.  :  FlexibleInstances - For Opaleye table types
--             :  MultiParamTypeClasses - For Opaleye table types
--             :  TemplateHaskell - Lets Opaleye generate the mapping function
--             :  NoImplicitPrelude - Use RIO instead
--             :  GeneralizedNewtypeDeriving - Simplify newtype usage
--             :  OverloadedStrings - Use Text literals
--
-- Database interface for the "follows" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Conduit.Persistence.FollowsTable
  ( Follows
  , allFollowsQ
  )
where

import qualified Data.Profunctor.Product        ( )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import qualified Opaleye                       as OE
import           Conduit.Persistence.DbConfig   ( schemaName )
import qualified Conduit.Persistence.UsersTable
                                               as PU
import           RIO

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "follows" table.
data FollowsT key
  = Follows
      { followerFk :: !key,
        followeeFk :: !key
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "follows" table.
type FollowsW = FollowsT PU.UserIdField -- user FK

-- | Record that Opaleye reads from the "follows" table.
type FollowsR = FollowsW

-- | Typesafe Haskell record to interface with the application.
type Follows = FollowsT PU.UserId -- user fk

instance Display Follows where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pFollows" ''FollowsT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
followsTable :: OE.Table FollowsW FollowsR
followsTable = OE.tableWithSchema
  schemaName
  "follows"
  (pFollows Follows
    { followerFk = PU.pUserId (PU.UserId (OE.tableField "follower_fk"))
    , followeeFk = PU.pUserId (PU.UserId (OE.tableField "followee_fk"))
    }
  )

--------------------
-- Basic Query
--------------------

-- | Retrieve all follow relations.
allFollowsQ :: OE.Select FollowsR
allFollowsQ = OE.selectTable followsTable
