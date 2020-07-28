{-# LANGUAGE DeriveGeneric #-}
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
--
-- Comment resource used in the Conduit ReST API
module Presenter.Resources.Comment
  ( Comment (..),
  )
where

import qualified Data.Aeson as J
import qualified Data.Time as DT
import qualified Presenter.Resources.Profile as RP
import RIO

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
    { id :: Integer,
     createdAt :: DT.UTCTime,
     updatedAt :: DT.UTCTime,
     body :: Text,
     author :: RP.Profile }
   deriving (Show, Eq, Generic)

instance J.ToJSON Comment
