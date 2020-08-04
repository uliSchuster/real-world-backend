{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--             :  GeneralizedNewtypeDeriving - Simplify newtype usage
--
-- Domain data types and business logic to describe a blog comment.
module Domain.Comment
  ( Comment(..)
  , CommentId
  , getCommentId
  , mkCommentIdFromInt64
  )
where

import qualified Data.Time                     as DT
import           Domain.Content
import           Domain.User
import           RIO

newtype CommentId = CommentId Integer
  deriving (Eq, Show, Display)

mkCommentIdFromInt64 :: Int64 -> CommentId
mkCommentIdFromInt64 cId = CommentId $ toInteger cId

getCommentId :: CommentId -> Integer
getCommentId (CommentId cId) = cId

data Comment
  = Comment
      { commentId :: !CommentId
      , commentBody :: !Body
      , commentCreatedAt :: !DT.UTCTime
      , commentModifiedAt :: !DT.UTCTime
      , commentAuthor :: !User
      }
  deriving (Eq, Show)
