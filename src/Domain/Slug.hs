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
--
-- Abstract data type for the article slug that identifies the corresponding
-- page. The slug here is modeled as opaque type that can only be created from
-- an article title. It relies on the contraints on character set and word
-- spacing enforced by the `Domain.Title` type, and adds a length constraint.
module Domain.Slug
  ( Slug (), -- Do not export data constructor.
    pattern Slug,
    getSlugText,
    mkSlug,
  )
where

import Domain.Title
import RIO
import qualified RIO.Text as T

-- | Maximum length of a slug. Must be limited because `Domain.Title` is not.
maxSlugLength :: Int
maxSlugLength = 200 -- Arbitrary but sufficiently large number

-- | Separator used to create the slug from the title text.
slugSeparator :: Text
slugSeparator = "-"

-- | Type that encapsulates a slug. Garanteed to have valid characters only.
-- Because the data constructor is not exported, slugs cannot be created
-- arbitrarily.
newtype Slug = UnconstrainedSlug Text
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Slug` type even though
-- the data constructor is hidden.
pattern Slug :: Text -> Slug
pattern Slug a <- UnconstrainedSlug a

-- To satisfy the completeness checker;
-- see https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE Slug #-}

getSlugText :: Slug -> Text
getSlugText (UnconstrainedSlug t) = t

-- | A slug is a lowercase version of the title string, limited in length and
-- with interword spaces replaced by the `slugSeparator`.
mkSlug :: Title -> Slug
mkSlug (Title t) =
  UnconstrainedSlug
    $ T.intercalate slugSeparator
      . T.words
      . T.toLower
      . T.take maxSlugLength
    $ t
