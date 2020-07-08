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
-- Abstract data type for a length-constrained user name.
-- This is an attempt to encode some constraints about the Username in its type
-- and to ensure those constraints by means of a smart constructor. It is a
-- concrete type for the Username only, it does not generalize to other text
-- strings: In particular: The username must be a single word with Latin-1
-- alphanumeric characters, with minimum and maximum length.
module Domain.Username
  ( UserName (), -- do not export the data constructor
    pattern UserName,
    getUserNameText,
    mkUserName,
  )
where

import RIO
import qualified RIO.Text as T
import qualified Text.Latin1 as TL1

-- | Limit minimum and maximum length of the user name so that it can function
-- as a unique identifier for users, for DB lookups, in authentication
-- operations, etc.
minUserNameLength :: Int
minUserNameLength = 5

maxUserNameLength :: Int
maxUserNameLength = 20

-- | Type that encapsulates a username string. Garanteed to have valid Latin-1
-- characters only, without whitespace.
-- Because the data constructor is not exported, usernames with arbitrary
-- Unicode characters cannot be constructed.
newtype UserName = UnconstrainedUserName Text
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `UserName` type even
-- though the data constructor is hidden.
pattern UserName :: Text -> UserName
pattern UserName a <- UnconstrainedUserName a

-- To satisfy the completeness checker;
-- see https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE UserName #-}

getUserNameText :: UserName -> Text
getUserNameText (UnconstrainedUserName t) = t

-- Smart constructor to guarantee a well-formed username string.
mkUserName :: Text -> Maybe UserName
mkUserName u
  | isValid canonicalU =
    Just (UnconstrainedUserName canonicalU)
  | otherwise = Nothing
  where
    canonicalU = T.strip u

-- | Validate a user name. For simplicity, do not return error messages.
isValid :: Text -> Bool
isValid u =
  validMinLength u
    && validMaxLength u
    && validCharacters u
    && validInitial u
  where
    validMinLength x = T.length x >= minUserNameLength
    validMaxLength x = T.length x <= maxUserNameLength
    validCharacters = T.all TL1.isPrintable
    validInitial = TL1.isAlpha . flip T.index 1
