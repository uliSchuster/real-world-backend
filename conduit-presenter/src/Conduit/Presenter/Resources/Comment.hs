{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
--             :  DeriveAnyClass - To include ToJSON in th deriving clause
--             :  InstanceSigs - Write signatures for class instance functions
--             :  MultiParamTypeClasses - The Resource class has two parameters
--
-- Comment resource used in the Conduit ReST API
module Conduit.Presenter.Resources.Comment
  ( Comment(..)
  , TR.toResource
  )
where

import qualified Data.Aeson                    as J
import qualified Data.Time                     as T
import qualified Conduit.Presenter.Resources.Profile
                                               as RP
import qualified Conduit.Presenter.Resources.ToResource
                                               as TR
import qualified Conduit.Domain.Comment        as DC
import qualified Conduit.Domain.Content        as DCO
import           RIO                     hiding ( id )

-- {
--   "comment": {
--     "id": 1,
--     "createdAt": "2016-02-18T03:22:56.637Z",
--     "updatedAt": "2016-02-18T03:22:56.637Z",
--     "body": "It takes a Jacobian",
--     "author": {
--       "username": "jake",
--       "bio": "I work at statefarm",
--       "image": "https://i.stack.imgur.com/xHWG8.jpg",
--       "following": false
--     }
--   }
-- }
data Comment = Comment
    { id :: !Integer,
     createdAt :: !T.UTCTime,
     updatedAt :: !T.UTCTime,
     body :: !Text,
     author :: !RP.Profile }
   deriving (Show, Eq, Generic, J.ToJSON)

instance TR.ToResource DC.Comment Comment where
  toResource :: DC.Comment -> Comment
  toResource dc = Comment { id        = DC.getCommentId . DC.commentId $ dc
                          , body      = DCO.getBody . DC.commentBody $ dc
                          , createdAt = DC.commentCreatedAt dc
                          , updatedAt = DC.commentModifiedAt dc
                          , author    = TR.toResource . DC.commentAuthor $ dc
                          }
