{-# LANGUAGE NoImplicitPrelude #-}

module AppConfig
  ( AppConfig(..)
  , HasDbConfig()
  , dBConfigL
  )
where

import qualified Conduit.Persistence.DbConfig  as DBC
import qualified Conduit.Persistence.API       as R
import qualified Conduit.Usecases.API          as U
import           RIO

-- | Genereic configuration data structure for the entire application. Each IO
-- subsystem has its own config hierarchy.
data AppConfig
  = AppConfig
      { dBConfig :: !DBC.DbConfig,
        logger :: !LogFunc
        -- wsConfig :: ???
      }

class HasDbConfig config where
  dBConfigL :: Lens' config DBC.DbConfig

instance HasDbConfig DBC.DbConfig where
  dBConfigL = id

instance HasDbConfig AppConfig where
  dBConfigL = lens dBConfig (\x y -> x { dBConfig = y })

instance DBC.HasDbConnInfo AppConfig where
  connInfoL = dBConfigL . DBC.connInfoL

instance DBC.HasDbConnPool AppConfig where
  connPoolL = dBConfigL . DBC.connPoolL

instance HasLogFunc AppConfig where
  logFuncL = lens logger (\x y -> x { logger = y })

-- Wire dependencies.
instance U.TagRepositoryI AppConfig where
  readTags = R.readTags

instance U.UserRepositoryI AppConfig where
  readUser = R.readUser

instance U.ArticleRepositoryI AppConfig where
  readArticles        = R.readArticles
  readArticle         = R.readArticle
  readArticleComments = R.readArticleComments
