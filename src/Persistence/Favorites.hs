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
-- Database interface for the "favorites" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.Favorites
  ( UserId (..),
    ArticleId (..),
    Favorite,
    getAllFavorites,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import Persistence.Articles (ArticleId (..))
import Persistence.PersistenceUtils
import Persistence.Users (UserId (..))
import RIO

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "favorites" table.
data FavoriteT userKey articleKey
  = Favorite
      { readerFk :: userKey,
        articleFk :: articleKey
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "favorites" table.
type FavoriteW =
  FavoriteT
    (F OE.SqlInt8) -- reader FK
    (F OE.SqlInt8) -- favorite FK

-- | Record that Opaleye reads from the "favorites" table.
type FavoriteR =
  FavoriteT
    (F OE.SqlInt8) -- reader FK
    (F OE.SqlInt8) -- favorite FK

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type Favorite =
  FavoriteT
    UserId -- reader
    ArticleId -- favorite

instance Display Favorite where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pFavorite" ''FavoriteT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
favoritesTable :: OE.Table FavoriteW FavoriteR
favoritesTable =
  OE.Table
    "favorites"
    ( pFavorite
        Favorite
          { readerFk = OE.required "reader_fk",
            articleFk = OE.required "favorite_fk"
          }
    )

--------------------
-- Queries
--------------------

-- | Retrieve all follow relations.
selectFavorites :: OE.Select FavoriteR
selectFavorites = OE.selectTable favoritesTable

--------------------
-- DB Access
--------------------

getAllFavorites :: PGS.ConnectInfo -> IO [Favorite]
getAllFavorites connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn selectFavorites
  PGS.close conn
  return result
