{-# LANGUAGE NoImplicitPrelude #-}
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Public interface of the Presenter library.
-- This is the Presenter _module_ according to clean architecture terminology.
module Conduit.Presenter.API
  (
  -- * Article resources
    PRA.Article(..)
  -- ** Resource for a liste of tags
  , PRT.TagList(..)
  -- ** Resource for an article comment
  , PRC.Comment
  -- * Resource for user profiles
  , PRP.Profile(..)
  -- * Genereic resource handling
  , PRR.Resource(..)
  -- * Type class to convert a domain model into coresponding resources
  , PRO.ToResource()
  , PRO.toResource
  )
where

import qualified Conduit.Presenter.Resources.Article
                                               as PRA
import qualified Conduit.Presenter.Resources.Comment
                                               as PRC
import qualified Conduit.Presenter.Resources.Profile
                                               as PRP
import qualified Conduit.Presenter.Resources.Resource
                                               as PRR
import qualified Conduit.Presenter.Resources.Tags
                                               as PRT
import qualified Conduit.Presenter.Resources.ToResource
                                               as PRO
