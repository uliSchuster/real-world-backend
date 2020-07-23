-- {-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Paths_real_world_server
import qualified Persistence.Users as PU
-- import RIO

connInfo :: PGS.ConnectInfo
connInfo =
  PGS.ConnectInfo
    { PGS.connectHost = "localhost",
      PGS.connectPort = 5432,
      PGS.connectDatabase = "conduit",
      PGS.connectPassword = "conduit",
      PGS.connectUser = "cond"
    }

main :: IO ()
main = do
  conn <- PGS.connect connInfo
  allUsers <- PU.getAllUsers conn
  print allUsers
