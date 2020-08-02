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
-- Database interface for the "articles" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.Articles
  ( ArticleIdT (..),
    ArticleIdField,
    OptionalArticleIdField,
    ArticleId,
    pArticleId,
    ArticleT (..),
    ArticleR,
    Article,
    allArticlesQ,
    allArticlesSortedQ,
    articlesCountSortedQ,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Time as T
import qualified Opaleye as OE
import Persistence.DbConfig (schemaName)
import Persistence.PersistenceUtils (F)
import qualified Persistence.Users as PU
import RIO

-----------------------
-- Dedicated Article ID
-----------------------
newtype ArticleIdT a = ArticleId {getArticleId :: a}
  deriving (Eq, Show, Display)

$(makeAdaptorAndInstance "pArticleId" ''ArticleIdT)

type ArticleIdField = ArticleIdT (F OE.SqlInt8)

type OptionalArticleIdField = ArticleIdT (Maybe (F OE.SqlInt8))

type ArticleId = ArticleIdT Int64

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "article" table.
data ArticleT aKey uKey title description body timestamp
  = Article
      { articleKey :: aKey,
        articleAuthorFk :: uKey,
        articleTitle :: title,
        articleDescription :: description,
        articleBody :: body,
        articleCreatedAt :: timestamp, -- TODO: Same type? Both optional?
        articleUpdatedAt :: timestamp
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "article" table.
type ArticleW =
  ArticleT
    OptionalArticleIdField -- autogenerated key
    PU.UserIdField -- author FK
    (F OE.SqlText) -- title
    (F OE.SqlText) -- description
    (F OE.SqlText) -- body
    (Maybe (F OE.SqlTimestamptz)) -- timestamps

-- | Record that Opaleye reads from the "articles" table.
type ArticleR =
  ArticleT
    ArticleIdField -- article key
    PU.UserIdField -- author FK
    (F OE.SqlText) -- title
    (F OE.SqlText) -- description
    (F OE.SqlText) -- body
    (F OE.SqlTimestamptz) -- timestamps

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type Article =
  ArticleT
    ArticleId -- article key
    PU.UserId -- author FK
    Text -- title
    Text -- description
    Text -- body
    T.UTCTime -- timestamps

instance Display Article where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pArticle" ''ArticleT)

-- | The actual mapping setup between the PostgreSQL records and the Haskell
-- record.
articlesTable :: OE.Table ArticleW ArticleR
articlesTable =
  OE.tableWithSchema
    schemaName
    "articles"
    ( pArticle
        Article
          { articleKey = pArticleId (ArticleId (OE.tableField "id")),
            articleAuthorFk = PU.pUserId (PU.UserId (OE.tableField "author_fk")),
            articleTitle = OE.tableField "title",
            articleDescription = OE.tableField "description",
            articleBody = OE.tableField "body",
            articleCreatedAt = OE.tableField "created_at",
            articleUpdatedAt = OE.tableField "updated_at"
          }
    )

--------------------
-- Queries
--------------------

-- | Retrieve all articles.
allArticlesQ :: OE.Select ArticleR
allArticlesQ = OE.selectTable articlesTable

-- | Retrieve all articles, sorted by modification date.
allArticlesSortedQ :: OE.Select ArticleR
allArticlesSortedQ = OE.orderBy (OE.desc articleCreatedAt) $ OE.selectTable articlesTable

-- | Retrieve the first count articles
articlesCountSortedQ :: Int -> OE.Select ArticleR
articlesCountSortedQ count = OE.limit count allArticlesSortedQ
