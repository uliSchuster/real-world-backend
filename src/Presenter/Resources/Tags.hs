{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--             :  DeriveGeneric - To automatically derive JSON mappers
--             :  InstanceSigs - Write signatures for class instance functions
--             :  MultiParamTypeClasses - The Resource class has two parameters
--             :  FlexibleInstances - Allow List instace
--
-- Tag and Tag-list Resources used in the Conduit ReST API
module Presenter.Resources.Tags
  ( TagList (..),
    TR.toResource,
  )
where

import qualified Data.Aeson as J
import qualified Domain.Tag as DT
import qualified Presenter.Resources.ToResource as TR
import RIO

newtype TagList = TagList {tags :: [Text]}
  deriving (Show, Eq, Generic)

instance J.ToJSON TagList

instance TR.ToResource [DT.Tag] TagList where
  toResource :: [DT.Tag] -> TagList
  toResource ts = TagList $ DT.getTag <$> ts
