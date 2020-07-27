{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--             :  FunctionalDependencies - for DTO mapping typclass
--             :  GeneralizedNewtypeDeriving - Derive typeclasses for newtypes
--             :  DeriveGeneric - To automatically derive JSON mappers
--
-- Gateway that converts domain types to JSON representation.
module Presenter.Json.JsonDto
  ( HasDto (),
    toDto,
    TagListDto (..),
  )
where

import qualified Data.Aeson as J
import qualified Domain.Tag as DT
import RIO

-- | Relation between domain type and DTO.
-- If the domain type @dom@ has a corresponding DTO @dt@,
-- as identified via a functional dependency, then the
-- two types can be mapped.
class Eq dto => HasDto dom dto where
  toDto :: dom -> dto

newtype TagListDto = TagListDto { tags :: [Text] }
  deriving (Show, Eq, Generic)

instance J.ToJSON TagListDto

instance HasDto [DT.Tag] TagListDto where
  toDto ts = TagListDto $ DT.getTag <$> ts
