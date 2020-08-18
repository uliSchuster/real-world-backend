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
-- The simplest Conduit use case: retrieve all stored tags.
-- Uses an abstract repository to retrieve the persisted tags. Filters them and
-- logs errors for those tags that could not be properly loaded from the store.
module Conduit.Usecases.TagUsecases
  ( getTags
  )
where

import qualified Conduit.Domain.API            as D
import           RIO
import           Conduit.Usecases.TagRepositoryI

-- | Use the tag-repository configured at application-level to read all tags.
-- For those tags that fail to map on the domain type, log an error.
getTags :: (HasLogFunc cfg, TagRepositoryI cfg) => RIO cfg [D.Tag]
getTags = do
  tagsErrors <- readTags
  mapM_ logInvalidTag tagsErrors
  return $ rights tagsErrors

-- | Helper function to log the error reported by the repository.
logInvalidTag :: (HasLogFunc cfg) => Either Text D.Tag -> RIO cfg ()
logInvalidTag it = case it of
  Right _ -> return ()
  Left  e -> logError $ display e
