{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
--             :  OverloadedStrings - Use Text literals
--
-- Abstract data type for canonical title text with constrained character set.
-- This is an attempt to encode some constraints about the Title in its type
-- and to ensure those constraints by means of a smart constructor. Similar to
-- the `Domain.Article` type, it does not generalize to other text strings.
-- The title must be Latin-1 alphanumeric or whitespace.
-- In addition, the title text is canonicalized: No leading or trailing
-- whitespace, exactly one whitespace character between words.
-- Because of the stringent invariants ensured by the `Title` type, it can be
-- converted into a `Slug` without additional validation.
module Domain.Title
  ( -- | Title
    Title()
  , pattern Title
  , getTitle
  , mkTitle
  , reconstructTitleFromSlug
  -- | Slug
  , maxSlugLength
  , slugSeparator
  , Slug()
  , pattern Slug
  , getSlug
  , mkSlug
  , mkSlugFromText
  )
where

import           Domain.ValidationUtils
import           RIO
import qualified RIO.Text                      as T


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

-- | Smart constructor to guarantee a well-formed title string.
mkTitle :: Text -> Maybe Title
mkTitle t = do
  vl <- valLatin1Letters t
  vn <- valNonEmpty vl
  return $ (Title_ . toCanonicText) vn

-- | Reconstruct a `Title` from a given slug.
-- This does not restore capitalization and dropped characters that got lost
-- when initially constructing a `Slug` from a `Title`.
reconstructTitleFromSlug :: Slug -> Title
reconstructTitleFromSlug (Slug slug) =
  Title_ $ T.unwords $ T.split (== slugSeparator) slug


-- | Maximum length of a slug. Must be limited because `Domain.Title` is not.
maxSlugLength :: Int
maxSlugLength = 200 -- Arbitrary but sufficiently large number

-- | Separator used to create the slug from the title text.
slugSeparator :: Char
slugSeparator = '-'

-- | Type that encapsulates a slug. Garanteed to have valid characters only.
-- Because the data constructor is not exported, slugs cannot be created
-- arbitrarily.
newtype Slug = Slug_ {getSlug :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Slug` type even though
-- the data constructor is hidden.
-- See https://stackoverflow.com/questions/33722381/pattern-matching-on-a-private-data-constructor
pattern Slug :: Text -> Slug
pattern Slug s <- Slug_ s

-- To satisfy the completeness checker;
-- see https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE Slug #-}

-- | A slug is a lowercase version of the title string, limited in length and
-- with interword spaces replaced by the `slugSeparator`.
mkSlug :: Title -> Slug
mkSlug (Title t) =
  Slug_
    $ T.intercalate (T.singleton slugSeparator)
    . T.words
    . T.toLower
    . T.take maxSlugLength
    $ t

-- | Create a slug from text input; e.g., from the UI.
-- Ensure that the text provided is a valid slug: It consists of words 
-- with admissible characters separated by the slug separator, nothing else.
mkSlugFromText :: Text -> Maybe Slug
mkSlugFromText t = do
  let t1 = T.toLower . T.strip $ t
  v1 <- valLength 1 maxSlugLength t1
  let w = T.split (== slugSeparator) v1
  v2 <- sequence $ valLatin1LettersNoWhitespace <$> w
  return $ Slug_ (T.intercalate (T.singleton ' ') v2)
