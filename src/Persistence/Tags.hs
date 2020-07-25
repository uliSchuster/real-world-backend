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
-- Database interface for the "tags" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.Tags
  ( TagId (..),
    Tag,
    getAllTags,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import Persistence.DbConfig (schemaName)
import Persistence.PersistenceUtils
import RIO

--------------------
-- Dedicated Tag ID
--------------------
newtype TagId = TagId {getTagId :: Int64}
  deriving (Show, Eq, Ord, Display, Num, Enum)

instance OE.QueryRunnerColumnDefault OE.SqlInt8 TagId where
  queryRunnerColumnDefault = TagId <$> OE.queryRunnerColumnDefault

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "tags" table.
data TagT tKey tName
  = Tag
      { tagKey :: tKey,
        tagName :: tName
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "tags" table.
type TagW =
  TagT
    (Maybe (F OE.SqlInt8)) -- autogenerated key
    (F OE.SqlText) -- tagname

-- | Record that Opaleye reads from the "tags" table.
type TagR =
  TagT
    (F OE.SqlInt8) -- tag key
    (F OE.SqlText) -- tagname

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type Tag =
  TagT
    TagId -- comment key
    Text -- tagname

instance Display Tag where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pTag" ''TagT)

-- | The actual mapping setup between the PostgreSQL records and the Haskell
-- record.
tagsTable :: OE.Table TagW TagR
tagsTable =
  OE.tableWithSchema
    schemaName
    "tags"
    ( pTag
        Tag
          { tagKey = OE.tableField "id",
            tagName = OE.tableField "tagname"
          }
    )

--------------------
-- Queries
--------------------

-- | Retrieve all comments.
selectTagss :: OE.Select TagR
selectTagss = OE.selectTable tagsTable

--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access.

getAllTags :: PGS.ConnectInfo -> IO [Tag]
getAllTags connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn selectTagss
  PGS.close conn
  return result
