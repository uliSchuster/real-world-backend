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
-- Database interface for the "articles_tags" relation, using the Opaleye
-- mapper and typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Conduit.Persistence.TaggedArticles
  ( TaggedArticleT(..)
  , TaggedArticleR
  , TaggedArticle
  , TagArrayT(..)
  , TagArrayF
  , TagArray
  , allTaggedArticlesQ
  , articleTagsQ
  , findAllTaggedArticles
  )
where

import           Control.Arrow                  ( (<<<)
                                                , arr
                                                , returnA
                                                )
import qualified Data.Profunctor.Product       as PP
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import qualified Database.PostgreSQL.Simple    as PGS
import qualified Opaleye                       as OE
import           Opaleye                        ( (.===) )
import qualified Conduit.Persistence.Articles  as PA
import           Conduit.Persistence.DbConfig   ( schemaName )
import           Conduit.Persistence.PersistenceUtils
import qualified Conduit.Persistence.Tags      as PT
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
-- Queries
--------------------

-- | Query all article-tag relations.
allTaggedArticlesQ :: OE.Select TaggedArticleR
allTaggedArticlesQ = OE.selectTable taggedArticlesTable

-- | Join article-ids and tag names for all articles.
articleTagNamesQ :: OE.Select (PA.ArticleIdField, F OE.SqlText)
articleTagNamesQ = proc () -> do
  TaggedArticle {articleFk = aId, tagFk = tFk} <- allTaggedArticlesQ -< ()
  PT.Tag {PT.tagKey = tId, PT.tagName = tn} <- PT.allTagsQ -< ()
  OE.restrict -< tFk .=== tId
  returnA -< (aId, tn)

-- | Aggregate all tag names for all articles
articleTagsQ :: OE.Select (F OE.SqlInt8, F (OE.SqlArray OE.SqlText))
articleTagsQ =
  OE.aggregate (PP.p2 (OE.groupBy, OE.arrayAgg)) $
    arr (first PA.getArticleId) <<< articleTagNamesQ

--------------------
-- DB Access
--------------------

-- TODO: Use Bracket or ResourceT to handle DB resources.

-- | Find all tagged articles stored in the DB and return them.
-- Naming convention: DB retrievals are called "find".
findAllTaggedArticles :: PGS.ConnectInfo -> IO [TaggedArticle]
findAllTaggedArticles connInfo = do
  conn   <- PGS.connect connInfo
  result <- OE.runSelect conn allTaggedArticlesQ
  PGS.close conn
  return result
