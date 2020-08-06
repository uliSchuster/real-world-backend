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
module Conduit.Persistence.PersistenceUtils
  ( F
  , FNull
  , withPostgreSQL
  )
where

import qualified Database.PostgreSQL.Simple    as PGS
import qualified Opaleye                       as OE
import           RIO

-- | Type synonyms for convenience
type F field = OE.Field field -- ^ Opaleye type for a non-nullable DB field.

type FNull field = OE.FieldNullable field   -- ^ Nullable DB field

-- | Safely execute a DB action: Properly acquire the DB connection and clean 
-- up the resources afterwards.
withPostgreSQL :: PGS.ConnectInfo -> (PGS.Connection -> IO a) -> IO a
withPostgreSQL connInfo = bracket (PGS.connect connInfo) -- acquire
                                  PGS.close -- release
