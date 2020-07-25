{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
--             :  MultiParamTypeClasses - Required by Opaleye
--             :  TemplateHaskell - Lets Opaleye generate the mapping function
--             :  NoImplicitPrelude - Use RIO instead
--             :  GeneralizedNewtypeDeriving - Simplify newtype usage
--
-- Database interface for the "comments" relation, using the Opaleye mapper and
-- typesafe query and data manipulation DSL.
-- See https://github.com/tomjaguarpaw/haskell-opaleye and the (outdated)
-- tutorial here: https://www.haskelltutorials.com/opaleye/index.html
module Persistence.Comments
  ( CommentId (..),
    ArticleId (..),
    UserId (..),
    Comment,
    getAllComments,
  )
where

import qualified Control.Arrow ()
import qualified Data.Profunctor.Product ()
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Time as T
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as OE
import Persistence.Articles (ArticleId (..))
import Persistence.PersistenceUtils
import Persistence.Users (UserId (..))
import RIO

-----------------------
-- Dedicated Comment ID
-----------------------
newtype CommentId = CommentId {getCommentId :: Int64}
  deriving (Show, Eq, Ord, Display, Num, Enum)

instance OE.QueryRunnerColumnDefault OE.SqlInt8 CommentId where
  queryRunnerColumnDefault = CommentId <$> OE.queryRunnerColumnDefault

--------------------
-- Table Setup
--------------------

-- | Polymorphic type for the "comment" table.
data CommentT cKey aKey uKey body timestamp
  = Comment
      { commentKey :: cKey,
        commentArticleFk :: aKey,
        commentAuthorFk :: uKey,
        commentBody :: body,
        commentCreatedAt :: timestamp, -- TODO: Same type? Both optional?
        commentUpdatedAt :: timestamp
      }
  deriving (Show)

-- | Record that Opaleye uses to write to the "comments" table.
type CommentW =
  CommentT
    (Maybe (F OE.SqlInt8)) -- autogenerated key
    (F OE.SqlInt8) -- article FK
    (F OE.SqlInt8) -- author FK
    (F OE.SqlText) -- body
    (Maybe (F OE.SqlTimestamptz)) -- timestamps

-- | Record that Opaleye reads from the "comments" table.
type CommentR =
  CommentT
    (F OE.SqlInt8) -- comment key
    (F OE.SqlInt8) -- article FK
    (F OE.SqlInt8) -- author FK
    (F OE.SqlText) -- body
    (F OE.SqlTimestamptz) -- timestamps

-- | Typesafe Haskell record to interface with the application. Under the hood,
-- Opaleye converts between this application record and the above PostgreSQL
-- read and write records.
type Comment =
  CommentT
    CommentId -- comment key
    ArticleId -- article FK
    UserId -- author FK
    Text -- body
    T.UTCTime -- timestamps

instance Display Comment where
  display = displayShow

-- | Template Haskell helper to create the mapping function between PostgreSQL
-- records and the Haskell record used below.
$(makeAdaptorAndInstance "pComment" ''CommentT)

-- | The actual mapping setup between the PostgreSQL records and the Haskell
-- record.
commentsTable :: OE.Table CommentW CommentR
commentsTable =
  OE.table
    "comments"
    ( pComment
        Comment
          { commentKey = OE.optional "id",
            commentArticleFk = OE.required "article_fk",
            commentAuthorFk = OE.required "author_fk",
            commentBody = OE.required "body",
            commentCreatedAt = OE.optional "created_at",
            commentUpdatedAt = OE.optional "updated_at"
          }
    )

--------------------
-- Queries
--------------------

-- | Retrieve all comments.
selectComments :: OE.Select CommentR
selectComments = OE.selectTable commentsTable

--------------------
-- DB Access
--------------------
-- Functions in the IO Monad that perform the actual database access.

getAllComments :: PGS.ConnectInfo -> IO [Comment]
getAllComments connInfo = do
  conn <- PGS.connect connInfo
  result <- OE.runSelect conn selectComments
  PGS.close conn
  return result
