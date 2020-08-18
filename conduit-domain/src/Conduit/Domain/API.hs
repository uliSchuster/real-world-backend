{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  PatternSynonyms - Re-export patterns for abstract types
--             :  NoImplicitPrelude - Use RIO instead
--
-- Public interface of the domain library.
-- This is the Domain _module_ according to clean architecture terminology.
module Conduit.Domain.API
  ( DA.Article(..)
  , DC.Comment(..)
  , DC.CommentId
  , DC.getCommentId
  , DC.mkCommentIdFromInt64
  , DO.Description(..)
  , DO.Body(..)
  , DG.Tag
  , pattern DG.Tag
  , DG.getTag
  , DG.mkTag
  , DT.Title
  , pattern DT.Title
  , DT.getTitle
  , DT.mkTitle
  , DT.reconstructTitleFromSlug
  , DT.Slug
  , DT.getSlug
  , DT.mkSlug
  , DT.mkSlugFromText
  , DU.UserBio
  , DU.getUserBio
  , DU.User(..)
  , DU.mkUser
  , DN.Username
  , pattern DN.Username
  , DN.getUsername
  , DN.mkUsername
  )
where

import qualified Conduit.Domain.Article        as DA
import qualified Conduit.Domain.Comment        as DC
import qualified Conduit.Domain.Content        as DO
import qualified Conduit.Domain.Tag            as DG
import qualified Conduit.Domain.Title          as DT
import qualified Conduit.Domain.User           as DU
import qualified Conduit.Domain.Username       as DN
