-- {-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified AppConfig as APC
import qualified Database.PostgreSQL.Simple as PGS
import qualified Paths_real_world_server
import qualified Persistence.Articles as PA
import qualified Persistence.Comments as PC
import qualified Persistence.DbConfig as DBC
import qualified Persistence.Favorites as PFA
import qualified Persistence.Follows as PFO
import qualified Persistence.TaggedArticles as PTA
import qualified Persistence.Tags as PT
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
  allFollows <- liftIO $ PFO.getAllFollows connInf
  logInfo $ mconcat (display <$> allFollows)
  allArticles <- liftIO $ PA.getAllArticles connInf
  logInfo $ mconcat (display <$> allArticles)
  allFavorites <- liftIO $ PFA.getAllFavorites connInf
  logInfo $ mconcat (display <$> allFavorites)
  allComments <- liftIO $ PC.getAllComments connInf
  logInfo $ mconcat (display <$> allComments)
  allTags <- liftIO $ PT.getAllTags connInf
  logInfo $ mconcat (display <$> allTags)
  allTaggedArticles <- liftIO $ PTA.getAllTaggedArticles connInf
  logInfo $ mconcat (display <$> allTaggedArticles)

-- | Enry point of the application
main :: IO ()
main = runApp conduitApp
