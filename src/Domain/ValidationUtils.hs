{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  OverloadedStrings - Use Text literals
--             :  NoImplicitPrelude - Use RIO instead
--
-- Functions to normalize and validate text input.
module Domain.ValidationUtils
  ( toCanonicText,
    valLatin1Letters,
    valLatin1PrintableNonSpace,
    valInitialLatin1Letter,
    valLength,
  )
where

import RIO
import qualified RIO.Text as T
import qualified Text.Latin1 as TL1

-- | Remove leading and trailing whitespace. Ensure that words in the string are
-- separated by exactly one space each.
toCanonicText :: Text -> Text
toCanonicText = T.intercalate " " . T.words . T.strip

-- | Ensure that the input text contains only alphanumeric and whitespace
-- Latin-1 characters.
valLatin1Letters :: Text -> Maybe Text
valLatin1Letters t
  | isValid t = Just t
  | otherwise = Nothing
  where
    isValid = T.all (\s -> TL1.isAlphaNum s || TL1.isWhiteSpace s)

-- | Ensure that the input text contains only printable Latin-1 characters, but
-- no whitespace.
valLatin1PrintableNonSpace :: Text -> Maybe Text
valLatin1PrintableNonSpace t
  | isValid t = Just t
  | otherwise = Nothing
  where
    isValid = T.all (\s -> TL1.isPrintable s && not (TL1.isWhiteSpace s))

-- | Ensure that the first character of the input string is alphanumeric.
valInitialLatin1Letter :: Text -> Maybe Text
valInitialLatin1Letter t
  | TL1.isAlpha . flip T.index 0 $ t = Just t
  | otherwise = Nothing

-- | Ensure that the input text satisfies the constraints on its minimum and
-- maximum length.
valLength :: Int -> Int -> Text -> Maybe Text
valLength minL maxL t
  | T.length t >= minL && T.length t <= maxL = Just t
  | otherwise = Nothing
