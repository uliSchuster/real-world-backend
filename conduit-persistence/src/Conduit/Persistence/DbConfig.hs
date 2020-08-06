{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Conduit.Persistence.DbConfig
  ( schemaName
  , DbConnectionInfo(..)
  , ConnPool(..)
  , DbConfig(..)
  , HasDbConnInfo()
  , HasDbConnPool()
  , connInfoL
  , connPoolL
  , initDb
  )
where

import qualified Database.PostgreSQL.Simple    as PGS
import qualified Data.Pool                     as DPL
import qualified RIO.Text                      as T
import           RIO

-- | Name of the PostgreSQL schema of the DB.
schemaName :: String
schemaName = "cond"

data DbConnectionInfo = DbConnectionInfo
  {
    host :: !Text
  , port :: Word16
  , database :: !Text
  , password :: !Text
  , user :: !Text}
  deriving (Eq, Show)


-- | The connection pool.
newtype ConnPool = ConnPool { getConnPool :: IO (DPL.Pool PGS.Connection) }

-- | Configuration of the PostgreSQL DBMS.
-- So far, we connect to the DB via a simple connection string.
-- TODO: Use connection pool.
data DbConfig = DbConfig
  { connInfo :: !PGS.ConnectInfo
  , connPool :: ConnPool }

class HasDbConnInfo dbConfig where
  connInfoL :: Lens' dbConfig PGS.ConnectInfo

class HasDbConnPool dbConfig where
  connPoolL :: Lens' dbConfig ConnPool

instance HasDbConnInfo PGS.ConnectInfo where
  connInfoL = id

instance HasDbConnPool ConnPool where
  connPoolL = id

instance HasDbConnInfo DbConfig where
  connInfoL = lens connInfo (\x y -> x { connInfo = y })

instance HasDbConnPool DbConfig where
  connPoolL = lens connPool (\x y -> x { connPool = y })

-- | Initialize the DB configuration environment that consists of the 
-- connection information and a connection pool.
-- TODO: Remove the connection information once the pool is working.
initDb :: DbConnectionInfo -> DbConfig
initDb cInf = DbConfig { connInfo = connectionInfo, connPool = connectionPool }
 where
  connectionInfo = PGS.ConnectInfo
    { PGS.connectHost     = T.unpack . host $ cInf
    , PGS.connectPort     = port cInf
    , PGS.connectDatabase = T.unpack . database $ cInf
    , PGS.connectPassword = T.unpack . password $ cInf
    , PGS.connectUser     = T.unpack . user $ cInf
    }
  connectionPool = ConnPool $ DPL.createPool (PGS.connect connectionInfo)
                                             PGS.close
                                             1 -- stripes
                                             60 -- keep unused connections open for 60s
                                             10 -- max. 10 connections open per stripe
