{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduit.Persistence.DbConfig
  ( schemaName
  , DbConfig(..)
  , HasDbConnInfo()
  , connInfoL
  )
where

import qualified Database.PostgreSQL.Simple    as PGS
import           RIO

-- | Name of the PostgreSQL schema of the DB.
schemaName :: String
schemaName = "cond"

-- | Configuration of the PostgreSQL DBMS.
-- So far, we connect to the DB via a simple connection string.
-- TODO: Use connection pool.
newtype DbConfig = DbConfig { connInfo :: PGS.ConnectInfo}
  deriving (Eq, Show)
  
class HasDbConnInfo dbConfig where
  connInfoL :: Lens' dbConfig PGS.ConnectInfo

instance HasDbConnInfo PGS.ConnectInfo where
  connInfoL = id

instance HasDbConnInfo DbConfig where
  connInfoL = lens connInfo (\x y -> x { connInfo = y })
