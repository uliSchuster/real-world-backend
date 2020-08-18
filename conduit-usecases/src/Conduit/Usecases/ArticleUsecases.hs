{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--             :  GeneralizedNewtypeDeriving - Simplify newtype usage
--             :  OverloadedStrings - Use Text literals
--
-- Database interface for the "articles_tags" relation, using the Opaleye
-- mapper and typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Conduit.Usecases.ArticleUsecases
  ( ArticleQueryOptions(..)
  , ArticleFilter(..)
  , getArticles
  , getArticle
  , getArticleComments
  )
where

import qualified Conduit.Domain.API            as D
import           RIO
import           Conduit.Usecases.ArticleRepositoryI

data ArticleFilter
  = ArticleFilter
      { author :: !(Maybe D.Username),
        favoritedBy :: !(Maybe D.Username),
        tag :: !(Maybe D.Tag)
      }
  deriving (Eq, Show)

data ArticleQueryOptions
  = ArticleQueryOptions
      { articleLimit :: !Int, -- TODO: Make Nonnegative
        articleOffset :: !Int, -- TODO: Make Nonnegative
        articleFilter :: !ArticleFilter
      }
  deriving (Eq, Show)

-- | Get a given number of articles from the repositiry, sorted by publication
-- date.
getArticles
  :: ArticleRepositoryI cfg
  => ArticleQueryOptions
  -> RIO cfg (Either Text [D.Article])
getArticles ArticleQueryOptions { articleLimit = limit, articleOffset = offset }
  = readArticles limit offset

-- | Get a specific article identified by its slug.
getArticle
  :: ArticleRepositoryI cfg => D.Slug -> RIO cfg (Either Text D.Article)
getArticle = readArticle

-- | Get all comments that pertain to an article that is identified by its slug.
getArticleComments
  :: ArticleRepositoryI cfg => D.Slug -> RIO cfg (Either Text [D.Comment])
getArticleComments = readArticleComments
