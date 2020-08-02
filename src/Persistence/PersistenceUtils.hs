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
-- Utility functions to simplify using Opaleye and PostfreSQL-simple
module Persistence.PersistenceUtils
  ( F
  , FNull
  )
where

import qualified Opaleye                       as OE

-- | Type synonyms for convenience
type F field = OE.Field field -- ^ Opaleye type for a non-nullable DB field.

type FNull field = OE.FieldNullable field   -- ^ Nullable DB field
