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
import qualified Conduit.Domain.API            as D
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
  :: (DBC.HasDbConnPool cfg)
  => Int -- ^ Number of articles to read. Must be less than `maxReadCount`.
  -> Int   -- ^ Offset to start reading from..
  -> RIO cfg (Either Text [D.Article])
readArticles limit offset = do
  connPool            <- view DBC.connPoolL
  articlesAuthorsTags <- liftIO
    $ findLimitedSortedArticlesWithAuthorsAndTags connPool limit
  return
    $   VAL.validationToEither
    .   sequenceA
    $   VAL.eitherToValidation
    .   toArticle
    <$> articlesAuthorsTags


-- | Read a single article identified by its slug from the repository.
-- The slug uniquely identifies an article.
readArticle
  :: (DBC.HasDbConnPool cfg)
  => D.Slug -- ^ Slug that uniquely identifies this article.
  -> RIO cfg (Either Text D.Article)
readArticle slug = do
  connPool       <- view DBC.connPoolL
  articleOrMaybe <- liftIO $ findArticleBySlug connPool slug
  case articleOrMaybe of
    Nothing ->
      return $ Left ("No article with slug " <> D.getSlug slug <> " found.")
    Just article -> return $ toArticle article

-- | Read all comments that pertain to a given article that is identified by 
-- its slug.
readArticleComments
  :: (DBC.HasDbConnPool cfg)
  => D.Slug -- ^ Slug that uniquely identifies the commented article.
  -> RIO cfg (Either Text [D.Comment])
readArticleComments slug = do
  connPool        <- view DBC.connPoolL
  commentsAuthors <- liftIO $ findArticleCommentsBySlug connPool slug
  return
    $   VAL.validationToEither
    .   sequenceA
    $   VAL.eitherToValidation
    .   toComment
    <$> commentsAuthors

toTitle :: Text -> PA.ArticleId -> Either Text D.Title
toTitle t tId =
  note
      (  "The article title "
      <> t
      <> " stored with database ID "
      <> tshow tId
      <> " is invalid."
      )
    $ D.mkTitle t

toUser :: PU.User -> Either Text D.User
toUser u =
  note
      (  "The user "
      <> PU.userUsername u
      <> " stored with database ID "
      <> tshow (PU.userKey u)
      <> " is invalid."
      )
    $ D.mkUser (PU.userEmail u)
               (PU.userUsername u)
               (PU.userImageUrl u)
               (PU.userBio u)

-- | Convert article, user and tag records retrieved from the database into an
-- `DA.Article` domain value.
toArticle :: (PA.Article, PU.User, PTA.TagArray) -> Either Text D.Article
toArticle (a, u, ts) =
  D.Article
    <$> toTitle (PA.articleTitle a) (PA.articleKey a)
    <*> Right (D.Description $ PA.articleDescription a)
    <*> Right (D.Body $ PA.articleBody a)
    <*> Right (PA.articleCreatedAt a)
    <*> Right (PA.articleUpdatedAt a)
    <*> toUser u
    <*> tagList
 where
  tagList = note "A persisted tag is invalid."
    $ sequence (D.mkTag <$> PTA.getTagArray ts)

-- | Convert comment and user records retrieved from the database into a
-- `DCO.Comment` domain value.
toComment :: (PC.Comment, PU.User) -> Either Text D.Comment
toComment (c, u) =
  D.Comment
    <$> Right (D.mkCommentIdFromInt64 . PC.getCommentId $ PC.commentKey c)
    <*> Right (D.Body $ PC.commentBody c)
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
  :: DBC.ConnPool -> Int -> IO [(PA.Article, PU.User, PTA.TagArray)]
findLimitedSortedArticlesWithAuthorsAndTags connPool limit =
  withPostgreSQLPool connPool $ \conn ->
    OE.runSelect conn $ articlesWithAuthorsAndTagsCountSortedQ limit

-- | Find an article by its slug and return the article jointly with its author 
-- and all associated tags.
findArticleBySlug
  :: DBC.ConnPool -> D.Slug -> IO (Maybe (PA.Article, PU.User, PTA.TagArray))
findArticleBySlug connPool slug = withPostgreSQLPool connPool $ \conn -> do
  result <- OE.runSelect
    conn
    (arr (const (OE.sqlStrictText rTitle)) >>> articlesWithAuthorAndTagsByTitleQ
    )
  return $ headMaybe result
  where rTitle = D.getTitle $ D.reconstructTitleFromSlug slug


-- | Find all comments that pertain to a given article identified by its slug. 
-- Together with each comment, return the comment's author.
findArticleCommentsBySlug
  :: DBC.ConnPool -> D.Slug -> IO [(PC.Comment, PU.User)]
findArticleCommentsBySlug connPool slug =
  withPostgreSQLPool connPool $ \conn ->
    OE.runSelect
      conn
      (arr (const (OE.sqlStrictText rTitle)) >>> articleCommentsByTitleQ) :: IO
        [(PC.Comment, PU.User)]
  where rTitle = D.getTitle $ D.reconstructTitleFromSlug slug

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
articleTagsQ :: OE.Select (PA.ArticleIdField, F (OE.SqlArray OE.SqlText))
articleTagsQ =
  OE.aggregate (PP.p2 (PA.pArticleId (PA.ArticleId OE.groupBy), OE.arrayAgg)) articleTagNamesQ

-- | Retrieve all articles with corresponding author and array of tags.
articlesWithAuthorsAndTagsQ :: OE.Select (PA.ArticleR, PU.UserR, PTA.TagArrayF)
articlesWithAuthorsAndTagsQ = proc () -> do
  (aId, ts) <- articleTagsQ -< ()
  article <- PA.allArticlesQ -< ()
  user <- PU.allUsersQ -< ()
  OE.restrict -< PA.articleKey article .=== aId
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
