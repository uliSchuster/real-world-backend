{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--             :  DeriveGeneric - To automatically derive JSON mappers
--             :  OverloadedStrings - Use Text literals
--
-- Resources used in the Conduit ReST API
module Conduit.Presenter.Resources.Resource
  ( Resource(..)
  )
where

import qualified Data.Aeson                    as J
import qualified Conduit.Presenter.Resources.Article
                                               as RA
import qualified Conduit.Presenter.Resources.Comment
                                               as RC
import qualified Conduit.Presenter.Resources.Profile
                                               as RP
import qualified Conduit.Presenter.Resources.Tags
                                               as RT
import           RIO

data Resource
  = Profile RP.Profile
  | Article RA.Article
  | Articles [RA.Article]
  | Comment RC.Comment
  | Comments [RC.Comment]
  | Tags RT.TagList
  deriving (Eq, Show, Generic)

instance J.ToJSON Resource where
  toJSON (Profile  p ) = J.object [("profile", J.toJSON p)]
  toJSON (Article  a ) = J.toJSON a
  toJSON (Articles as) = J.object [("articles", J.toJSON as)]
  toJSON (Comment  c ) = J.toJSON c
  toJSON (Comments cs) = J.object [("comments", J.toJSON cs)]
  toJSON (Tags     t ) = J.toJSON t
