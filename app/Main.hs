-- {-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified AppConfig as APC
import qualified Database.PostgreSQL.Simple as PGS
import qualified Paths_real_world_server
import qualified Persistence.DbConfig as DBC
import qualified Persistence.Users as PU
import RIO

-- | Connection string for the PostgreSQL DBMS. Here for development and 
-- experimentation. Should be loaded from a config file or the system environment later on.
dBConnInfo :: DBC.DbConfig
dBConnInfo =
  DBC.DbConfig $
    PGS.ConnectInfo
      { PGS.connectHost = "localhost",
        PGS.connectPort = 5432,
        PGS.connectDatabase = "conduit",
        PGS.connectPassword = "conduit",
        PGS.connectUser = "cond"
      }

-- Helper function to run the main application logic.
-- This function is a very preliminary sketch. The goal later on is to properly 
-- isolate the different IO subsystems (logging, command line, web server, DB).
runApp :: RIO APC.AppConfig a -> IO a
runApp app = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let cfg =
          APC.AppConfig
            { APC.dBConfig = dBConnInfo,
              APC.logger = logFunc
            }
    runRIO cfg app

-- In the full-fledged app, this will be application's main event loop.
-- For now, we just play with the persistence subsystem to get it working.
conduitApp :: RIO APC.AppConfig ()
conduitApp = do
  connInf <- view $ APC.dbConfigL . DBC.connInfoL
  allUsers <- liftIO $ PU.getAllUsers connInf
  logInfo $ mconcat (display <$> allUsers)

-- | Enry point of the application
main :: IO ()
main = runApp conduitApp
