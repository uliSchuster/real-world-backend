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
-- Domain data types and business logic to describe a blog comment.
module Domain.Comment
  ( Comment (..),
    -- Reexport for convenience
    module Domain.Content,
    module Domain.User,
  )
where

import qualified Data.Time as DT
import Domain.Article
import Domain.Content
import Domain.User
import RIO

data Comment
  = Comment
      { commentBody :: Body,
        commentCreatedAt :: DT.UTCTime,
        commentModifiedAt :: DT.UTCTime,
        commentAuthor :: User
      }
  deriving (Eq, Show)
