{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Clean-Architecture-Style dependency inversion: The inner use case ring
-- should not depend on the implementation of the persistence mechanism.
-- Therefore, the persistence interface is defined here in the form of a type -- class.
module Usecases.ArticleRepositoryI
  ( ArticleRepositoryI()
  , readArticles
  , readArticle
  )
where

import qualified Domain.Article                as DA
import qualified Domain.Title                  as DT
import           RIO

-- | Interface of the actual persistence engine employed by this use case.
-- Must be implemented by the outermost application ring.
class ArticleRepositoryI articleRepo where
  readArticles
    :: Int -- ^ Number of articles to read. Must be less than
    -> Int -- ^ Offset to start reading from. Must be <= `maxReadCount`.
    -> RIO articleRepo (Either Text [DA.Article])
  readArticle
    :: DT.Slug -- ^ Slug that uniquely identifies this article.
    -> RIO articleRepo (Either Text DA.Article)
