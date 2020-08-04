{-# LANGUAGE OverloadedStrings #-}
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
module Usecases.ArticleUsecases
  ( ArticleQueryOptions(..)
  , ArticleFilter(..)
  , getArticles
  , getArticle
  , getArticleComments
  )
where

import qualified Domain.Article                as DA
import qualified Domain.Comment                as DC
import qualified Domain.Tag                    as DTG
import qualified Domain.Username               as DU
import qualified Domain.Title                  as DT
import           RIO
import           Usecases.ArticleRepositoryI

data ArticleFilter
  = ArticleFilter
      { author :: !(Maybe DU.Username),
        favoritedBy :: !(Maybe DU.Username),
        tag :: !(Maybe DTG.Tag)
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
  -> RIO cfg (Either Text [DA.Article])
getArticles ArticleQueryOptions { articleLimit = limit, articleOffset = offset }
  = readArticles limit offset

-- | Get a specific article identified by its slug.
getArticle
  :: ArticleRepositoryI cfg => DT.Slug -> RIO cfg (Either Text DA.Article)
getArticle = readArticle

-- | Get all comments that pertain to an article that is identified by its slug.
getArticleComments
  :: ArticleRepositoryI cfg => DT.Slug -> RIO cfg (Either Text [DC.Comment])
getArticleComments = readArticleComments