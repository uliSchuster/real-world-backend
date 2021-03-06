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
-- Database interface for the "articles_tags" relation, using the Opaleye
-- mapper and typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Conduit.Persistence.TaggedArticlesTable
  ( TaggedArticleT(..)
  , TaggedArticleR
  , TaggedArticle
  , TagArrayT(..)
  , TagArrayF
  , TagArray
  , allTaggedArticlesQ
  )
where

import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import qualified Opaleye                       as OE
import qualified Conduit.Persistence.ArticlesTable
                                               as PA
import qualified Conduit.Persistence.TagsTable as PT
import           Conduit.Persistence.DbConfig   ( schemaName )
import           Conduit.Persistence.PersistenceUtils
import           RIO

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "favorites" table.
data TaggedArticleT articleKey tagKey
  = TaggedArticle
      { articleFk :: !articleKey,
        tagFk :: !tagKey
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "favorites" table.
type TaggedArticleW
  = TaggedArticleT PA.ArticleIdField -- article FK
                                     PT.TagIdField -- tag FK

-- | Record that Opaleye reads from the "favorites" table.
type TaggedArticleR = TaggedArticleW

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type TaggedArticle
  = TaggedArticleT PA.ArticleId -- article FK
                                PT.TagId -- tag FK

instance Display TaggedArticle where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pTaggedArticle" ''TaggedArticleT)

-- | The actual mapping setup tells Opaleye exactly how to map between the
-- PostgreSQL records and the Haskell record. For each record, the function
-- specifies the name of the table column and the constraints.
taggedArticlesTable :: OE.Table TaggedArticleW TaggedArticleR
taggedArticlesTable = OE.tableWithSchema
  schemaName
  "articles_tags"
  (pTaggedArticle TaggedArticle
    { articleFk = PA.pArticleId (PA.ArticleId (OE.tableField "article_fk"))
    , tagFk     = PT.pTagId (PT.TagId (OE.tableField "tag_fk"))
    }
  )

-- | To aggregate an article's tags into an array.
newtype TagArrayT a = TagArray {getTagArray :: a}
  deriving (Eq, Show, Display)

$(makeAdaptorAndInstance "pTagArray" ''TagArrayT)

type TagArrayF = TagArrayT (F (OE.SqlArray OE.SqlText))

type TagArray = TagArrayT [Text]

--------------------
-- Basic Query
--------------------

-- | Query all article-tag relations.
allTaggedArticlesQ :: OE.Select TaggedArticleR
allTaggedArticlesQ = OE.selectTable taggedArticlesTable
