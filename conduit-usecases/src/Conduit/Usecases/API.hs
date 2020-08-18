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
  ( ARI.ArticleRepositoryI()
  , ARI.readArticle
  , ARI.readArticles
  , ARI.readArticleComments
  , TRI.TagRepositoryI()
  , TRI.readTags
  , URI.UserRepositoryI()
  , URI.readUser
  , AUC.ArticleQueryOptions(..)
  , AUC.ArticleFilter(..)
  , AUC.getArticles
  , AUC.getArticle
  , AUC.getArticleComments
  , PUC.getProfile
  , TUC.getTags
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
