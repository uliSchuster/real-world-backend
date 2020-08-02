module AppConfig
  ( AppConfig(..)
  , HasDbConfig()
  , dbConfigL
  )
where

import qualified Persistence.ArticleRepository as ARepo
import qualified Persistence.DbConfig          as DBC
import qualified Persistence.TagRepository     as TRepo
import qualified Persistence.UserRepository    as URepo
import           RIO
import qualified Usecases.ArticleRepositoryI   as ARepoI
import qualified Usecases.TagRepositoryI       as TRepoI
import qualified Usecases.UserRepositoryI      as URepoI

-- | Genereic configuration data structure for the entire application. Each IO
-- subsystem has its own config hierarchy.
data AppConfig
  = AppConfig
      { dBConfig :: !DBC.DbConfig,
        logger :: !LogFunc
        -- wsConfig :: ???
      }

class HasDbConfig config where
  dbConfigL :: Lens' config DBC.DbConfig

instance HasDbConfig DBC.DbConfig where
  dbConfigL = id

instance HasDbConfig AppConfig where
  dbConfigL = lens dBConfig (\x y -> x { dBConfig = y })

instance DBC.HasDbConnInfo AppConfig where
  connInfoL = dbConfigL . DBC.connInfoL

instance HasLogFunc AppConfig where
  logFuncL = lens logger (\x y -> x { logger = y })

-- Wire dependencies.
instance TRepoI.TagRepositoryI AppConfig where
  readAllTags = TRepo.readAllTags

instance URepoI.UserRepositoryI AppConfig where
  readUser = URepo.readUser

instance ARepoI.ArticleRepositoryI AppConfig where
  readArticles = ARepo.readArticles
