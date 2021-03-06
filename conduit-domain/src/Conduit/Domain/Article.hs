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
module Conduit.Domain.Article
  ( Article(..)
  )
where

import qualified Data.Time                     as T
import qualified Conduit.Domain.Content        as DC
import qualified Conduit.Domain.Tag            as DTG
import qualified Conduit.Domain.Title          as DT
import qualified Conduit.Domain.User           as DU
import           RIO

-- | The `Article` type is a record with strongly typed fields. There is no
-- field for the article's slug, because the slug can be computed on the fly
-- from the `articleTitle`.
-- To identify the author of an article, a complete `User` type is embedded in
-- the `Article`, not just some key or other database reference. This does not
-- imply a de-normalized database model, but it simplifies dealing with articles
-- in memory, because the `User` is always required at the ReST interface.
data Article
  = Article
      { articleTitle :: !DT.Title,
        articleDescription :: !DC.Description,
        articleBody :: !DC.Body,
        articleCreatedAt :: !T.UTCTime,
        articleModifiedAt :: !T.UTCTime,
        articleAuthor :: !DU.User,
        articleTags :: ![DTG.Tag]
      }
  deriving (Eq, Show)
