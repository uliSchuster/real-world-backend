{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  GeneralizedNewtypeDeriving - Derive typeclasses for newtypes
--             :  NoImplicitPrelude - Use RIO instead
--
-- Domain data types for blog contents.
module Conduit.Domain.Content
  ( Description(..)
  , Body(..)
  )
where

import           RIO

-- | Wrapper type for the description of an article. So far, there are no
-- constraints on the text that makes up the description.
-- TODO: Consider sanitizing the description upon construction to prevent SQL
-- injections or illegal HTML tags.
newtype Description = Description {getDescription :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Wrapper tye for the actual article body. Similar to the `Description`,
-- there are no constraints on the text the body may contain.
newtype Body = Body {getBody :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)
