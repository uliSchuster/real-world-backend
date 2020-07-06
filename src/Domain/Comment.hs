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
-- Domain data types and business logic to describe a blog comment.
module Domain.Comment
  ( Comment (..),
  )
where

import qualified Data.Time as DT
import qualified Domain.Article as ART
import qualified Domain.User as USR
import RIO

data Comment
  = Comment
      { commendBody :: ART.ContentBody,
        commentCreatedAt :: DT.UTCTime,
        commentModifiedAt :: DT.UTCTime,
        commentUser :: USR.User
      }
