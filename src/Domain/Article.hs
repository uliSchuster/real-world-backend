{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Domain data types and business logic to describe a blog article.
module Domain.Article
  ( Article (..),
    -- Reexports for simplicity
    module Domain.Content,
    module Domain.Title,
    module Domain.User,
    module Domain.Slug,
  )
where

import qualified Data.Time as DT
import Domain.Content
import Domain.Slug
import Domain.Title
import Domain.User
import RIO

-- | The `Article` type is a record with strongly typed fields. There is no
-- field for the article's slug, because the slug can be computed on the fly
-- from the `articleTitle`.
-- To identify the author of an article, a complete `User` type is embedded in
-- the `Article`, not just some key or other database reference. This does not
-- imply a de-normalized database model, but it simplifies dealing with articles
-- in memory, because the `User` is always required at the ReST interface.
data Article
  = Article
      { articleTitle :: Title,
        articleDescription :: Description,
        articleBody :: Body,
        articleCreatedAt :: DT.UTCTime,
        articleModifiedAt :: DT.UTCTime,
        articleAuthor :: User
      }
  deriving (Eq, Show)
