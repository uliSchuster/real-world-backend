{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  GeneralizedNewtypeDeriving - Derive typeclasses for newtypes
--             :  PatternSynonyms - Set up custom patterns to match over
--             :  NoImplicitPrelude - Use RIO instead
--
-- Abstract data type for canonical article tags text with constrained
-- character set and maximum length, similar to `Domain.UserName`.
-- The tag must be Latin-1 alphanumeric or whitespace of fixed max length.
-- In addition, the title text is canonicalized: No leading or trailing
-- whitespace, exactly one whitespace character between words.
module Conduit.Domain.Tag
  ( minTagLength
  , maxTagLength
  , Tag(Tag)
  , getTag
  , mkTag
  )
where

import           Conduit.Domain.ValidationUtils
import           RIO

minTagLength :: Int
minTagLength = 3

maxTagLength :: Int
maxTagLength = 20

valTagLength :: Text -> Maybe Text
valTagLength = valLength minTagLength maxTagLength

-- | Type that encapsulates a tag string.
-- Because the data constructor is not exported, tags with arbitrary Unicode
-- characters cannot be constructed.
newtype Tag = Tag_ {getTag :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Title` type even though
-- the data constructor is hidden.
-- See https://stackoverflow.com/questions/33722381/pattern-matching-on-a-private-data-constructor
pattern Tag :: Text -> Tag
pattern Tag t <- Tag_ t

-- -- To satisfy the completeness checker;
-- -- see https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE Tag #-}

mkTag :: Text -> Maybe Tag
mkTag t = do
  canonical <- toCanonicText <$> valLatin1Letters t
  Tag_ <$> valTagLength canonical
