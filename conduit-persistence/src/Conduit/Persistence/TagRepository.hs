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
--             :  OverloadedStrings - Use Text literals
--
-- Abstract collection-like interface to the underlying persistence layer.
module Conduit.Persistence.TagRepository
  ( readAllTags
  )
where

import qualified Conduit.Domain.Tag            as DT
import qualified Conduit.Persistence.DbConfig  as DBC
import qualified Conduit.Persistence.Tags      as PT
import           RIO

-- | Returns all tags stored in the underlying persistence mechanism
-- Naming convention: Read repository operations are called "read".
readAllTags :: (DBC.HasDbConnInfo cfg) => RIO cfg [Either Text DT.Tag]
readAllTags = do
  connInfo <- view DBC.connInfoL
  pTags    <- liftIO $ PT.findAllTags connInfo
  return $ toTag <$> pTags

-- | Helper function that converts the data obtained from the DB into a
-- corresponding domain object. Because the domain types are shielded through
-- smart constructors, we need to handle the case here that a tag does not
-- conform to the domain restrictions. For now, we do it in the simples
-- possible way: return an error message.
toTag :: PT.Tag -> Either Text DT.Tag
toTag (PT.Tag (PT.TagId dbId) pt) = case DT.mkTag pt of
  Just tag -> Right tag
  Nothing ->
    Left
      $  "The tag "
      <> pt
      <> " at database ID "
      <> tshow dbId
      <> " is invalid."
