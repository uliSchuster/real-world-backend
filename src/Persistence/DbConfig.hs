module Persistence.DbConfig
  ( DbConfig (..),
    HasDbConnInfo (),
    connInfoL
  )
where

import qualified Database.PostgreSQL.Simple as PGS
import RIO

-- | Configuration of the PostgreSQL DBMS.
-- So far, we connect to the DB via a simple connection string.
-- TODO: Use connection pool.
newtype DbConfig
  = DbConfig
      { connInfo :: PGS.ConnectInfo
      }

class HasDbConnInfo dbConfig where
  connInfoL :: Lens' dbConfig PGS.ConnectInfo

instance HasDbConnInfo PGS.ConnectInfo where
  connInfoL = id

instance HasDbConnInfo DbConfig where
  connInfoL = lens connInfo (\x y -> x {connInfo = y})
