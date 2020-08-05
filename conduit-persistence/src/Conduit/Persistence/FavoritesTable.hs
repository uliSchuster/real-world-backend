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
-- Database interface for the "favorites" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Conduit.Persistence.FavoritesTable
  ( Favorite
  , allFavoritesQ
  )
where

import qualified Data.Profunctor.Product        ( )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import qualified Opaleye                       as OE
import qualified Conduit.Persistence.ArticlesTable
                                               as PA
import           Conduit.Persistence.DbConfig   ( schemaName )
import qualified Conduit.Persistence.UsersTable
                                               as PU
import           RIO

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "favorites" table.
data FavoriteT userKey articleKey
  = Favorite
      { readerFk :: !userKey,
        articleFk :: !articleKey
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "favorites" table.
type FavoriteW
  = FavoriteT PU.UserIdField -- reader FK
                             PA.ArticleIdField -- favorite FK

-- | Record that Opaleye reads from the "favorites" table.
type FavoriteR = FavoriteW

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type Favorite
  = FavoriteT PU.UserId -- reader
                        PA.ArticleId -- favorite

instance Display Favorite where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pFavorite" ''FavoriteT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
favoritesTable :: OE.Table FavoriteW FavoriteR
favoritesTable = OE.tableWithSchema
  schemaName
  "favorites"
  (pFavorite Favorite
    { readerFk  = PU.pUserId (PU.UserId (OE.tableField "reader_fk"))
    , articleFk = PA.pArticleId (PA.ArticleId (OE.tableField "favorite_fk"))
    }
  )

--------------------
-- Basic Query
--------------------

-- | Retrieve all follow relations.
allFavoritesQ :: OE.Select FavoriteR
allFavoritesQ = OE.selectTable favoritesTable

--------------------
-- DB Access
--------------------

-- | Find all favorites stored in the DB and return them.
-- Naming convention: DB retrievals are called "find".
-- findAllFavorites :: PGS.ConnectInfo -> IO [Favorite]
-- findAllFavorites connInfo = do
--   conn   <- PGS.connect connInfo
--   result <- OE.runSelect conn allFavoritesQ
--   PGS.close conn
--   return result
