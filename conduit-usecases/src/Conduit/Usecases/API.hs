{-# LANGUAGE NoImplicitPrelude #-}
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Public interface of the Usecases library.
-- This is the Usecases _module_ according to clean architecture terminology.
module Conduit.Usecases.API
  (
  -- * The application's use cases
  -- ** Use cases to manipulate articles
    AUC.ArticleQueryOptions(..)
  , AUC.ArticleFilter(..)
  , AUC.getArticles
  , AUC.getArticle
  , AUC.getArticleComments
  -- ** Use cases to manipulate user profiles
  , PUC.getProfile
  -- ** Use cases to manipulate tags
  , TUC.getTags
  -- * Type classes as repository interfaces for dependency inversion
  -- ** Interface for the article repository
  , ARI.ArticleRepositoryI()
  , ARI.readArticle
  , ARI.readArticles
  , ARI.readArticleComments
  -- ** Interface for the tag repository
  , TRI.TagRepositoryI()
  , TRI.readTags
  -- ** Interface for the user repository
  , URI.UserRepositoryI()
  , URI.readUser
  )
where

import qualified Conduit.Usecases.ArticleRepositoryI
                                               as ARI
import qualified Conduit.Usecases.TagRepositoryI
                                               as TRI
import qualified Conduit.Usecases.UserRepositoryI
                                               as URI
import qualified Conduit.Usecases.ArticleUsecases
                                               as AUC
import qualified Conduit.Usecases.ProfileUsecases
                                               as PUC
import qualified Conduit.Usecases.TagUsecases  as TUC
