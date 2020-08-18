{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  )
where

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Encode.Pretty      as JPP

import qualified Conduit.Domain.API            as D
import qualified Conduit.Usecases.API          as U
import qualified Conduit.Persistence.DbConfig  as DBC
import qualified Conduit.Presenter.API         as P
import qualified Conduit.UI.CommandLine.CLI    as CLI
import qualified AppConfig                     as APC
import           RIO
import           RIO.ByteString.Lazy            ( toStrict )

-- | Simple error type with error message, to be returned to the user.
-- This error handling is preliminary. It must be improved to allow for
-- comprehensive application-wide errors.
data ApplicationError
  = Unauthorized Text
  | Forbidden Text
  | NotFound Text
  | Invalid Text
  deriving (Eq, Ord, Show, Generic)

instance J.ToJSON ApplicationError

-- | Connection string for the PostgreSQL DBMS. Here for development and
-- experimentation. Should be loaded from a config file or the system environment later on.
dBConnInfo :: DBC.DbConnectionInfo
dBConnInfo = DBC.DbConnectionInfo { DBC.host     = "localhost"
                                  , DBC.port     = 5432
                                  , DBC.database = "conduit"
                                  , DBC.password = "conduit"
                                  , DBC.user     = "cond"
                                  }

-- Helper function to run the main application logic.
-- This function is a very preliminary sketch. The goal later on is to properly
-- isolate the different IO subsystems (logging, command line, web server, DB).
runApp :: RIO APC.AppConfig a -> IO a
runApp app = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let cfg = APC.AppConfig { APC.dBConfig = DBC.initDb dBConnInfo
                            , APC.logger   = logFunc
                            }
    runRIO cfg app

-- In the full-fledged app, this will be application's main event loop.
-- For now, we just play with the persistence subsystem to get it working.
conduitApp :: RIO APC.AppConfig ()
conduitApp = do
  request <- liftIO CLI.parseCmdLine
  result  <- case request of
    CLI.User                 -> undefined
    (CLI.Profile profileCmd) -> case profileCmd of
      (CLI.ShowProfile uName) -> getProfileResource uName
    (CLI.Article  slug       ) -> getArticleResource slug
    (CLI.Articles articlesCmd) -> case articlesCmd of
      (CLI.GetArticlesCmd aQuery) -> getArticleResources aQuery
    CLI.Comment         -> undefined
    (CLI.Comments slug) -> getArticleCommentsResource slug
    CLI.Tags            -> getTagResource
  let dispResult = displayResult result
  logInfo dispResult

-- connInf <- view $ APC.dbConfigL . DBC.connInfoL
-- allUsers <- liftIO $ PU.findAllUsers connInf
-- logInfo $ mconcat (display <$> allUsers)
-- allFollows <- liftIO $ PFO.findAllFollows connInf
-- logInfo $ mconcat (display <$> allFollows)
-- allArticles <- liftIO $ PA.findAllArticles connInf
-- logInfo $ mconcat (display <$> allArticles)
-- allFavorites <- liftIO $ PFA.findAllFavorites connInf
-- logInfo $ mconcat (display <$> allFavorites)
-- allComments <- liftIO $ PC.findAllComments connInf
-- logInfo $ mconcat (display <$> allComments)
-- allTags <- liftIO $ PT.findAllTags connInf
-- logInfo $ mconcat (display <$> allTags)
-- allTaggedArticles <- liftIO $ PTA.findAllTaggedArticles connInf
-- logInfo $ mconcat (display <$> allTaggedArticles)

-- | Enry point of the application
main :: IO ()
main = runApp conduitApp

getProfileResource
  :: D.Username -> RIO APC.AppConfig (Either ApplicationError P.Resource)
getProfileResource uName = do
  userOrError <- U.getProfile uName
  case userOrError of
    Left  e -> return $ Left $ NotFound e
    Right u -> return $ Right $ P.ResProfile $ P.toResource u

getTagResource :: RIO APC.AppConfig (Either ApplicationError P.Resource)
getTagResource = do
  dTags <- U.getTags
  let tagList = P.toResource dTags
  return $ Right (P.ResTags tagList) -- TODO: Error handling.

getArticleResource
  :: D.Slug -> RIO APC.AppConfig (Either ApplicationError P.Resource)
getArticleResource slug = do
  articleOrError <- U.getArticle slug
  case articleOrError of
    Left  e -> return $ Left $ NotFound e
    Right a -> return $ Right (P.ResArticle $ P.toResource a)

getArticleResources
  :: U.ArticleQueryOptions
  -> RIO APC.AppConfig (Either ApplicationError P.Resource)
getArticleResources qOpts = do
  articlesOrError <- U.getArticles qOpts
  case articlesOrError of
    Left  e  -> return $ Left $ NotFound e
    Right as -> return $ Right $ P.ResArticles $ P.toResource <$> as

getArticleCommentsResource
  :: D.Slug -> RIO APC.AppConfig (Either ApplicationError P.Resource)
getArticleCommentsResource slug = do
  commentsOrError <- U.getArticleComments slug
  case commentsOrError of
    Left  e  -> return $ Left $ NotFound e
    Right cs -> return $ Right $ P.ResComments $ P.toResource <$> cs


displayResult :: (J.ToJSON l, J.ToJSON r) => Either l r -> Utf8Builder
displayResult res = displayBytesUtf8 . toStrict $ out
 where
  out = case res of
    Left  e -> JPP.encodePretty e
    Right r -> JPP.encodePretty r
