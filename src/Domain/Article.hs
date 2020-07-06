{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Portability :  portable
--
-- Domain data types and business logic to describe a blog article.
module Domain.Article
  ( ContentBody (..),
    Article (..),
  )
where

import qualified Data.Time as DT
import qualified Domain.User as USR
import RIO

newtype ArticleTitle = ArticleTitle {getTitle :: Text}

newtype ArticleDescription = ArticleDescription {getDescription :: Text}

newtype ArticleSlug = ArticleSlug {getSlug :: Text}

newtype ContentBody = ArticleBody {getBody :: Text}

data Article
  = Article
      { articleTitle :: ArticleTitle,
        articleDescription :: ArticleDescription,
        articleSlug :: ArticleSlug,
        articleBody :: ContentBody,
        articleCreatedAt :: DT.UTCTime,
        articleModifiedAt :: DT.UTCTime,
        articleUser :: USR.User
      }
