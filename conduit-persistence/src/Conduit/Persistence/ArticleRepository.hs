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
module Conduit.Persistence.ArticleRepository
  ( maxReadCount
  , readArticles
  , readArticle
  , readArticleComments
  )
where

import           Control.Arrow                  ( returnA
                                                , arr
                                                , (<<<)
                                                )
import qualified Data.Profunctor.Product       as PP
import qualified Database.PostgreSQL.Simple    as PGS
import qualified Opaleye                       as OE
import           Opaleye                        ( (.==)
                                                , (.===)
                                                , (.++)
                                                )
import           Conduit.Persistence.PersistenceUtils
import qualified Conduit.Persistence.DbConfig  as DBC
import qualified Conduit.Persistence.ArticlesTable
                                               as PA
import qualified Conduit.Persistence.CommentsTable
                                               as PC
import qualified Conduit.Persistence.TagsTable as PT
import qualified Conduit.Persistence.TaggedArticlesTable
                                               as PTA
import qualified Conduit.Persistence.UsersTable
                                               as PU
import qualified Conduit.Domain.Article        as DA
import qualified Conduit.Domain.Comment        as DCO
import qualified Conduit.Domain.Content        as DC
import qualified Conduit.Domain.Tag            as DTG
import qualified Conduit.Domain.Title          as DT
import qualified Conduit.Domain.User           as DU
import qualified Data.Either.Validation        as VAL
import           Control.Error.Util             ( note )
import           RIO
import           RIO.List                       ( headMaybe )



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
readArticles
  :: (DBC.HasDbConnInfo cfg)
  => Int -- ^ Number of articles to read. Must be less than `maxReadCount`.
  -> Int   -- ^ Offset to start reading from..
  -> RIO cfg (Either Text [DA.Article])
readArticles limit offset = do
  connInfo            <- view DBC.connInfoL
  articlesAuthorsTags <- liftIO
    $ findLimitedSortedArticlesWithAuthorsAndTags connInfo limit
  return
    $   VAL.validationToEither
    .   sequenceA
    $   VAL.eitherToValidation
    .   toArticle
    <$> articlesAuthorsTags

-- | Read a single article identified by its slug from the repository.
-- The slug uniquely identifies an article.
readArticle
  :: (DBC.HasDbConnInfo cfg)
  => DT.Slug -- ^ Slug that uniquely identifies this article.
  -> RIO cfg (Either Text DA.Article)
readArticle slug = do
  connInfo       <- view DBC.connInfoL
  articleOrMaybe <- liftIO $ findArticleBySlug connInfo slug
  case articleOrMaybe of
    Nothing ->
      return $ Left ("No article with slug " <> DT.getSlug slug <> " found.")
    Just article -> return $ toArticle article

-- | Read all comments that pertain to a given article that is identified by 
-- its slug.
readArticleComments
  :: (DBC.HasDbConnInfo cfg)
  => DT.Slug -- ^ Slug that uniquely identifies the commented article.
  -> RIO cfg (Either Text [DCO.Comment])
readArticleComments slug = do
  connInfo        <- view DBC.connInfoL
  commentsAuthors <- liftIO $ findArticleCommentsBySlug connInfo slug
  return
    $   VAL.validationToEither
    .   sequenceA
    $   VAL.eitherToValidation
    .   toComment
    <$> commentsAuthors

toTitle :: Text -> PA.ArticleId -> Either Text DT.Title
toTitle t tId =
  note
      (  "The article title "
      <> t
      <> " stored with database ID "
      <> tshow tId
      <> " is invalid."
      )
    $ DT.mkTitle t

toUser :: PU.User -> Either Text DU.User
toUser u =
  note
      (  "The user "
      <> PU.userUsername u
      <> " stored with database ID "
      <> tshow (PU.userKey u)
      <> " is invalid."
      )
    $ DU.mkUser (PU.userEmail u)
                (PU.userUsername u)
                (PU.userImageUrl u)
                (PU.userBio u)

-- | Convert article, user and tag records retrieved from the database into an
-- `DA.Article` domain value.
toArticle :: (PA.Article, PU.User, PTA.TagArray) -> Either Text DA.Article
toArticle (a, u, ts) =
  DA.Article
    <$> toTitle (PA.articleTitle a) (PA.articleKey a)
    <*> Right (DC.Description $ PA.articleDescription a)
    <*> Right (DC.Body $ PA.articleBody a)
    <*> Right (PA.articleCreatedAt a)
    <*> Right (PA.articleUpdatedAt a)
    <*> toUser u
    <*> tagList
 where
  tagList = note "A persisted tag is invalid."
    $ sequence (DTG.mkTag <$> PTA.getTagArray ts)

-- | Convert comment and user records retrieved from the database into a
-- `DCO.Comment` domain value.
toComment :: (PC.Comment, PU.User) -> Either Text DCO.Comment
toComment (c, u) =
  DCO.Comment
    <$> Right (DCO.mkCommentIdFromInt64 . PC.getCommentId $ PC.commentKey c)
    <*> Right (DC.Body $ PC.commentBody c)
    <*> Right (PC.commentCreatedAt c)
    <*> Right (PC.commentUpdatedAt c)
    <*> toUser u


--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access.
-- Naming convention: DB retrievals are called "find".

-- | Join article with its author and its tags. Return a sorted list with the 
-- first `limit` articles, sorted by creation data with the newest article 
-- first.
findLimitedSortedArticlesWithAuthorsAndTags
  :: PGS.ConnectInfo -> Int -> IO [(PA.Article, PU.User, PTA.TagArray)]
findLimitedSortedArticlesWithAuthorsAndTags connInfo limit = do
  conn   <- PGS.connect connInfo
  result <- OE.runSelect conn $ articlesWithAuthorsAndTagsCountSortedQ limit
  PGS.close conn
  return result

-- | Find an article by its slug and return the article jointly with its author 
-- and all associated tags.
findArticleBySlug
  :: PGS.ConnectInfo
  -> DT.Slug
  -> IO (Maybe (PA.Article, PU.User, PTA.TagArray))
findArticleBySlug connInfo slug = do
  conn   <- PGS.connect connInfo
  result <-
    OE.runSelect
      conn
      (   arr (const (OE.sqlStrictText rTitle))
      >>> articlesWithAuthorAndTagsByTitleQ
      ) :: IO [(PA.Article, PU.User, PTA.TagArray)]
  PGS.close conn
  return $ headMaybe result
 where
  titleFromSlug = DT.reconstructTitleFromSlug slug
  rTitle        = DT.getTitle titleFromSlug

-- | Find all comments that pertain to a given article identified by its slug. 
-- Together with each comment, return the comment's author.
findArticleCommentsBySlug
  :: PGS.ConnectInfo -> DT.Slug -> IO [(PC.Comment, PU.User)]
findArticleCommentsBySlug connInfo slug = do
  conn   <- PGS.connect connInfo
  result <-
    OE.runSelect
      conn
      (arr (const (OE.sqlStrictText rTitle)) >>> articleCommentsByTitleQ) :: IO
      [(PC.Comment, PU.User)]
  PGS.close conn
  return result
 where
  titleFromSlug = DT.reconstructTitleFromSlug slug
  rTitle        = DT.getTitle titleFromSlug


--------------------
-- Compound Queries
--------------------

-- | Join article-ids and tag names for all articles.
articleTagNamesQ :: OE.Select (PA.ArticleIdField, F OE.SqlText)
articleTagNamesQ = proc () -> do
  PTA.TaggedArticle {PTA.articleFk = aId, PTA.tagFk = tFk} <- PTA.allTaggedArticlesQ -< ()
  PT.Tag {PT.tagKey = tId, PT.tagName = tn} <- PT.allTagsQ -< ()
  OE.restrict -< tFk .=== tId
  returnA -< (aId, tn)

-- | Aggregate all tag names for all articles
articleTagsQ :: OE.Select (F OE.SqlInt8, F (OE.SqlArray OE.SqlText))
articleTagsQ =
  OE.aggregate (PP.p2 (OE.groupBy, OE.arrayAgg)) $
    arr (first PA.getArticleId) <<< articleTagNamesQ

-- | Retrieve all articles with corresponding author and array of tags.
articlesWithAuthorsAndTagsQ :: OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
articlesWithAuthorsAndTagsQ = proc () -> do
  (aId, ts) <- articleTagsQ -< ()
  article <- PA.allArticlesQ -< ()
  user <- PU.allUsersQ -< ()
  OE.restrict -< (PA.getArticleId . PA.articleKey) article .== aId
  OE.restrict -< PA.articleAuthorFk article .=== PU.userKey user
  returnA -< (article, user, PTA.TagArray ts)

-- | Retrieve all articles with associated tags, sorted by modification date.
allArticlesWithAuthorsAndTagsSortedQ
  :: OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
allArticlesWithAuthorsAndTagsSortedQ = OE.orderBy
  (OE.desc (\(a, _, _) -> PA.articleCreatedAt a))
  articlesWithAuthorsAndTagsQ

-- | Retrieve the first `count` articles with associated tags.
articlesWithAuthorsAndTagsCountSortedQ
  :: Int -> OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
articlesWithAuthorsAndTagsCountSortedQ count =
  OE.limit count allArticlesWithAuthorsAndTagsSortedQ

-- | Query for articles whose title starts with the title provided as paramter.
-- Case insensitive search.
articlesWithAuthorAndTagsByTitleQ :: OE.SelectArr (F OE.SqlText) (PA.ArticleR, PU.UserR, PTA.TagArrayF)
articlesWithAuthorAndTagsByTitleQ = proc title -> do
  (art, auth, tags) <- articlesWithAuthorsAndTagsQ -< ()
  OE.restrict -< OE.ilike (PA.articleTitle art) (title .++ OE.sqlStrictText "%")
  returnA -< (art, auth, tags)


articleCommentsByTitleQ :: OE.SelectArr (F OE.SqlText) (PC.CommentR, PU.UserR)
articleCommentsByTitleQ = proc title -> do
  article <- PA.allArticlesQ -< ()
  comment <- PC.allCommentsQ -< ()
  author <- PU.allUsersQ -< ()
  OE.restrict -< OE.ilike (PA.articleTitle article) (title .++ OE.sqlStrictText "%")
  OE.restrict -< PA.articleKey article .=== PC.commentArticleFk comment
  OE.restrict -< PC.commentAuthorFk comment .=== PU.userKey author
  returnA -< (comment, author)
