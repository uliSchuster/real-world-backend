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
-- Database interface for the "articles_tags" relation, using the Opaleye
-- mapper and typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.TaggedArticles
  ( TagId (..),
    ArticleId (..),
    TaggedArticle,
    getAllTaggedArticles,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import Persistence.Articles (ArticleId (..))
import Persistence.DbConfig (schemaName)
import Persistence.PersistenceUtils
import Persistence.Tags (TagId (..))
import RIO

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "favorites" table.
data TaggedArticleT articleKey tagKey
  = TaggedArticle
      { articleFk :: articleKey,
        tagFk :: tagKey
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "favorites" table.
type TaggedArticleW =
  TaggedArticleT
    (F OE.SqlInt8) -- article FK
    (F OE.SqlInt8) -- tag FK

-- | Record that Opaleye reads from the "favorites" table.
type TaggedArticleR =
  TaggedArticleT
    (F OE.SqlInt8) -- article FK
    (F OE.SqlInt8) -- tag FK

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type TaggedArticle =
  TaggedArticleT
    ArticleId -- article FK
    TagId -- tag FK

instance Display TaggedArticle where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pTaggedArticle" ''TaggedArticleT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
taggedArticlesTable :: OE.Table TaggedArticleW TaggedArticleR
taggedArticlesTable =
  OE.tableWithSchema
    schemaName
    "articles_tags"
    ( pTaggedArticle
        TaggedArticle
          { articleFk = OE.required "article_fk",
            tagFk = OE.required "tag_fk"
          }
    )

--------------------
-- Queries
--------------------

-- | Retrieve all follow relations.
selectTaggedArticles :: OE.Select TaggedArticleR
selectTaggedArticles = OE.selectTable taggedArticlesTable

--------------------
-- DB Access
--------------------

getAllTaggedArticles :: PGS.ConnectInfo -> IO [TaggedArticle]
getAllTaggedArticles connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn selectTaggedArticles
  PGS.close conn
  return result
