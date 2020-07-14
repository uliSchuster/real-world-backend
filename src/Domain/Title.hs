{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
--             :  OverloadedStrings - Use Text literals
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
    getTitleText,
    mkTitle,
  )
where

import RIO
import qualified RIO.Text as T
import qualified Text.Latin1 as TL1

-- | Type that encapsulates a title string. Garanteed to have valid characters
-- only.
-- Because the data constructor is not exported, titles with arbitrary Unicode
-- characters cannot be constructed.
newtype Title = UnconstrainedTitle Text
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Title` type even though
-- the data constructor is hidden.
-- See https://stackoverflow.com/questions/33722381/pattern-matching-on-a-private-data-constructor
pattern Title :: Text -> Title
pattern Title t <- UnconstrainedTitle t

-- To satisfy the completeness checker;
-- see https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE Title #-}

getTitleText :: Title -> Text
getTitleText (UnconstrainedTitle t) = t

-- Smart constructor to guarantee a well-formed title string.
mkTitle :: Text -> Maybe Title
mkTitle t
  | areCharactersAdmissible t =
    Just (UnconstrainedTitle . canonicalizeTitle $ t)
  | otherwise = Nothing

-- | Ensure that the input text contains only alphanumeric and whitespace
-- Latin-1 characters.
areCharactersAdmissible :: Text -> Bool
areCharactersAdmissible = T.all (\t -> TL1.isAlphaNum t || TL1.isWhiteSpace t)

-- | Remove leading and trailing whitespace. Ensure that words in the title are
-- separated by exactly one space each.
canonicalizeTitle :: Text -> Text
canonicalizeTitle = T.intercalate " " . T.words . T.strip
