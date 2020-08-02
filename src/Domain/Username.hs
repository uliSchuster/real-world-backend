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
-- Abstract data type for a length-constrained user name.
-- This is an attempt to encode some constraints about the Username in its type,
-- and to ensure those constraints by means of a smart constructor.
-- This is a rather simple, non-generic variant of an abstract data type that
-- only works for a user name. It does not generalize to other Text strings.
-- The username must be a single word with Latin-1 alphanumeric characters
-- and between some minimum and maximum length.
module Domain.Username
  ( Username()
  , -- Do not export the data constructor
    pattern Username
  , getUsername
  , mkUsername
  , minUsernameLength
  , maxUsernameLength
  )
where

import           Domain.ValidationUtils
import           RIO
import qualified RIO.Text                      as T

-- | Limit minimum and maximum length of the user name so that it can function
-- as a unique identifier for users, for DB lookups, in authentication
-- operations, etc.
minUsernameLength :: Int
minUsernameLength = 5

maxUsernameLength :: Int
maxUsernameLength = 20

valUsernameLength :: Text -> Maybe Text
valUsernameLength = valLength minUsernameLength maxUsernameLength

-- | Type that encapsulates a username string. Garanteed to have valid Latin-1
-- characters only, without whitespace.
-- Because the data constructor is not exported, usernames with arbitrary
-- Unicode characters cannot be constructed.
newtype Username = Username_ {getUsername :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Username` type, even
-- though the data constructor is hidden.
-- See https://stackoverflow.com/questions/33722381/pattern-matching-on-a-private-data-constructor
pattern Username :: Text -> Username
pattern Username name <- Username_ name

-- To satisfy the completeness checker.
-- See https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
{-# COMPLETE Username #-}

-- Smart constructor to guarantee a well-formed username string.
-- A username must consist of printable Latin-1 characters, not contain spaces,
-- and adhere to the length constraints. Furthermore, we remove all leading and
-- trailing space from the input string.
mkUsername :: Text -> Maybe Username
mkUsername u = do
  canonical  <- valLatin1PrintableNonSpace . T.strip $ u
  valInitial <- valInitialLatin1Letter canonical
  Username_ <$> valUsernameLength valInitial
