{-# LANGUAGE NoImplicitPrelude #-}
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Public interface of the Persistence library.
-- This is the Persistence _module_ according to clean architecture terminology.
module Conduit.Persistence.API
  ( PAR.readArticles
  , PAR.readArticle
  , PAR.readArticleComments
  , PTR.readTags
  , PUR.readUser
  )
where

import qualified Conduit.Persistence.ArticleRepository
                                               as PAR
import qualified Conduit.Persistence.TagRepository
                                               as PTR
import qualified Conduit.Persistence.UserRepository
                                               as PUR
