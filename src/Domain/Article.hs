{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Domain.Slug
import Domain.Title
import Domain.User
import RIO

newtype ArticleDescription = ArticleDescription {getDescription :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

newtype ContentBody = ArticleBody {getBody :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

data Article
  = Article
      { articleTitle :: Title,
        articleDescription :: ArticleDescription,
        articleSlug :: Slug,
        articleBody :: ContentBody,
        articleCreatedAt :: DT.UTCTime,
        articleModifiedAt :: DT.UTCTime,
        articleUser :: User
      }
  deriving (Eq, Show)
