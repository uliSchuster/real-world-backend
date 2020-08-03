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
-- Abstract data type for canonical title text with constrained character set.
-- This is an attempt to encode some constraints about the Title in its type
-- and to ensure those constraints by means of a smart constructor. Similar to
-- the `Domain.Article` type, it does not generalize to other text strings.
-- The title must be Latin-1 alphanumeric or whitespace.
-- In addition, the title text is canonicalized: No leading or trailing
-- whitespace, exactly one whitespace character between words.
module Domain.Title
  ( Title (), -- do not export the data constructor
    pattern Title,
    getTitle,
    mkTitle,
  )
where

import Domain.ValidationUtils
import RIO

-- | Type that encapsulates a title string. Garanteed to have valid characters
-- only.
-- Because the data constructor is not exported, titles with arbitrary Unicode
-- characters cannot be constructed.
newtype Title = Title_ {getTitle :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Title` type even though
-- the data constructor is hidden.
-- See https://stackoverflow.com/questions/33722381/pattern-matching-on-a-private-data-constructor
pattern Title :: Text -> Title
pattern Title t <- Title_ t

-- To satisfy the completeness checker;
-- see https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE Title #-}

-- Smart constructor to guarantee a well-formed title string.
mkTitle :: Text -> Maybe Title
mkTitle t = Title_ . toCanonicText <$> valLatin1Letters t
