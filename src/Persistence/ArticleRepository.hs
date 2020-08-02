{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  Arrows - Required by Opaleye
--             :  FlexibleInstances - Required by Opaleye
--             :  NoImplicitPrelude - Use RIO instead
--             :  OverloadedStrings - Use Text literals
--
-- Abstract collection-like interface to the underlying persistence layer.
module Persistence.ArticleRepository
  ( maxReadCount,
    readArticles,
    toDomain,
    findAllArticlesSortedByModification,
    findArticlesCountSortedByModification,
    findLimitedSortedArticlesWithAuthorsAndTags
  )
where

import Control.Arrow (returnA)
import Control.Error.Util (note)
import qualified Data.Either.Validation as VAL
import qualified Database.PostgreSQL.Simple as PGS
import qualified Domain.Article as DA
import qualified Domain.Content as DC
import qualified Domain.Tag as DTG
import qualified Domain.Title as DT
import qualified Domain.User as DU
import qualified Opaleye as OE
import Opaleye ((.==), (.===))
import qualified Persistence.Articles as PA
import qualified Persistence.DbConfig as DBC
import qualified Persistence.TaggedArticles as PTA
import qualified Persistence.Tags as PT
import qualified Persistence.Users as PU
import RIO

-- | Maximum number of articles that can be read at a time.
maxReadCount :: Int
maxReadCount = 100

-- | Read a list of articles from the repository.
-- Articles are sorted according to their creation date, with the newest
-- article first. The caller must provide paging information: how many articles
-- to read at a time, and from which index to start reading.
-- Returns an empty list if there are no articles to read, or if the offset it
-- past the last article in the repository.
-- TODO: Make offset queries work.
readArticles ::
  (DBC.HasDbConnInfo cfg) =>
  -- | Number of articles to read. Must be less than
  Int ->
  -- | Offset to start reading from. Must be <= `maxReadCount`.
  Int ->
  RIO cfg (Either Text [DA.Article])
readArticles limit offset = do
  connInfo <- view DBC.connInfoL
  articlesAuthorsTags <- liftIO $ findLimitedSortedArticlesWithAuthorsAndTags connInfo limit
  return $ VAL.validationToEither . sequenceA $ VAL.eitherToValidation . toDomain <$> articlesAuthorsTags

-- | Convert records retrieved from the database into an `DA.Article` domein
-- value.
toDomain :: (PA.Article, PU.User, PTA.TagArray) -> Either Text DA.Article
toDomain (a, u, ts) =
  DA.Article
    <$> title
    <*> Right (DC.Description $ PA.articleDescription a)
    <*> Right (DC.Body $ PA.articleBody a)
    <*> Right (PA.articleCreatedAt a)
    <*> Right (PA.articleUpdatedAt a)
    <*> user
    <*> tagList
  where
    tagList = note "A persisted tag is invalid." $ sequence (DTG.mkTag <$> PTA.getTagArray ts)
    user =
      note ("The user " <> PU.userUsername u <> " stored with database ID " <> tshow (PU.userKey u) <> " is invalid.") $
        DU.mkUser
          (PU.userEmail u)
          (PU.userUsername u)
          (PU.userImageUrl u)
          (PU.userBio u)
    title = note ("The article title " <> PA.articleTitle a <> " stored with database ID " <> tshow (PA.articleKey a) <> " is invalid.") $ DT.mkTitle (PA.articleTitle a)

--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access.

-- | Find all articles stored in the DB and return them.
-- Sorted by descending modification date (youngest first).
-- Naming convention: DB retrievals are called "find".
-- This function might return a very large number of articles. Prefer the
-- constrained search.
findAllArticlesSortedByModification :: PGS.ConnectInfo -> IO [PA.Article]
findAllArticlesSortedByModification connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn PA.allArticlesSortedQ
  PGS.close conn
  return result

findArticlesCountSortedByModification :: PGS.ConnectInfo -> Int -> IO [PA.Article]
findArticlesCountSortedByModification connInfo count = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn $ PA.articlesCountSortedQ count
  PGS.close conn
  return result

findLimitedSortedArticlesWithAuthorsAndTags :: PGS.ConnectInfo -> Int -> IO [(PA.Article, PU.User, PTA.TagArray)]
findLimitedSortedArticlesWithAuthorsAndTags connInfo limit = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn $ articlesWithAuthorsAndTagsCountSortedQ limit
  PGS.close conn
  return result

--------------------
-- Complex Queries
--------------------
articlesWithAuthorQ :: Int -> OE.Select (PA.ArticleR, PU.UserR)
articlesWithAuthorQ count = proc () -> do
  articles <- PA.articlesCountSortedQ count -< ()
  users <- PU.allUsersQ -< ()
  OE.restrict -< (PU.getUserId . PA.articleAuthorFk $ articles) .== (PU.getUserId . PU.userKey $ users)
  returnA -< (articles, users)

tagsForArticlesQ :: OE.Select (PT.TagR, PTA.TaggedArticleR)
tagsForArticlesQ = proc () -> do
  tags <- PT.allTagsQ -< ()
  taggedArticles <- PTA.allTaggedArticlesQ -< ()
  OE.restrict -< (PT.getTagId . PT.tagKey $ tags) .== (PT.getTagId . PTA.tagFk $ taggedArticles)
  returnA -< (tags, taggedArticles)

-- | Retrieve all articles with corresponding author and array of tags.
articlesWithAuthorsAndTagsQ :: OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
articlesWithAuthorsAndTagsQ = proc () -> do
  (aId, ts) <- PTA.articleTagsQ -< ()
  article <- PA.allArticlesQ -< ()
  user <- PU.allUsersQ -< ()
  OE.restrict -< (PA.getArticleId . PA.articleKey) article .== aId
  OE.restrict -< PA.articleAuthorFk article .=== PU.userKey user
  returnA -< (article, user, PTA.TagArray ts)

-- | Retrieve all articles with associated tags, sorted by modification date.
allArticlesWithAuthorsAndTagsSortedQ :: OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
allArticlesWithAuthorsAndTagsSortedQ = OE.orderBy (OE.desc (\(a, _, _) -> PA.articleCreatedAt a)) articlesWithAuthorsAndTagsQ

-- | Retrieve the first `count` articles with associated tags.
articlesWithAuthorsAndTagsCountSortedQ :: Int -> OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
articlesWithAuthorsAndTagsCountSortedQ count = OE.limit count allArticlesWithAuthorsAndTagsSortedQ
