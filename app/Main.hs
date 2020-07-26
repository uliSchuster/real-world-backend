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
import qualified Persistence.TagRepository as TRepo
import qualified Persistence.TaggedArticles as PTA
import qualified Persistence.Tags as PT
import qualified Persistence.Users as PU
import RIO
import qualified RIO.Text as T
import qualified Usecases.TagRepositoryI as TRepoI
import qualified Usecases.TagUsecase as UCT

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

-- Wire dependencies.
instance TRepoI.TagRepositoryI APC.AppConfig where
  readAllTags = TRepo.readAllTags

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
  allUsers <- liftIO $ PU.findAllUsers connInf
  logInfo $ mconcat (display <$> allUsers)
  allFollows <- liftIO $ PFO.findAllFollows connInf
  logInfo $ mconcat (display <$> allFollows)
  allArticles <- liftIO $ PA.findAllArticles connInf
  logInfo $ mconcat (display <$> allArticles)
  allFavorites <- liftIO $ PFA.findAllFavorites connInf
  logInfo $ mconcat (display <$> allFavorites)
  allComments <- liftIO $ PC.findAllComments connInf
  logInfo $ mconcat (display <$> allComments)
  allTags <- liftIO $ PT.findAllTags connInf
  logInfo $ mconcat (display <$> allTags)
  allTaggedArticles <- liftIO $ PTA.findAllTaggedArticles connInf
  logInfo $ mconcat (display <$> allTaggedArticles)
  allDTags <- UCT.getAllTags
  logInfo $ display $ T.unwords (tshow <$> allDTags)

-- | Enry point of the application
main :: IO ()
main = runApp conduitApp
