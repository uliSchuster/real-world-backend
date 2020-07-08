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
-- Abstract data type for a length-constrained title text.
-- This is an attempt to encode some constraints about the Title in its type 
-- and ensure those constraints by means of a smart constructor. It is a 
-- concrete type for the Title only, it does not generalize to other text
-- strings: In particular, the length limits are hardcoded.
module Domain.Title
  ( Title (), -- do not export the data constructor
    pattern Title,
    getTitleText,
    mkTitle
  )
where

import RIO
import qualified RIO.Text as T

-- | A valid title must have at least 3 characters.
minTitleLength :: Int
minTitleLength = 3

-- | A valid title must not be longer than 100 characters.
maxTitleLength :: Int
maxTitleLength = 100

-- | Type that encapsulates a title string. Garanteed to be no longer than
-- `maxTitleLength` characters.
-- Because the data constructor is not exported, no arbitrary-length titles can
-- be constructed; instead, the smart constructor `mkTitel` must be used.
newtype Title = UnconstrainedTitle Text
  deriving (Eq, Show, Ord, IsString, Display, Hashable)

-- | Pattern synonym to allow pattern matching on the `Title` type even though
-- the data constructor is hidden.
pattern Title :: Text -> Title
pattern Title a <- UnconstrainedTitle a

getTitleText :: Title -> Text
getTitleText (UnconstrainedTitle t) = t

-- Smart constructor to guarantee the maximum length of the title string.
mkTitle :: Text -> Maybe Title
mkTitle t
  | T.length t > maxTitleLength && T.length t <= minTitleLength = Nothing
  | otherwise = Just (UnconstrainedTitle t)
