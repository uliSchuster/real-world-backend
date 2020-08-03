{-# LANGUAGE FunctionalDependencies #-}
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
--             :  FunctionalDependencies - for DTO mapping typclass
--
-- Resources used in the Conduit ReST API
module Presenter.Resources.ToResource
  ( ToResource
  , toResource
  )
where

-- | A type class for resources.
class ToResource dom res | res -> dom where
  -- | A resource can be created from a matchin set domain values.
  toResource :: dom -> res
