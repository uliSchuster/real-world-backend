{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.TestTag where

import qualified Data.Maybe as DM
import qualified Domain.DomainInstances as DI
import Domain.Tag
import RIO
import qualified RIO.Text as T
import qualified Test.Tasty.HUnit as HU
import qualified TestUtils as TU
import qualified Text.Latin1 as TL1

-- HUnit test cases --

unit_validTag :: IO ()
unit_validTag = HU.assertBool "Can a valid tag be constructed?" (isJust $ mkTag "testTag")

unit_tagTooShort :: IO ()
unit_tagTooShort = HU.assertBool "A tag that is less than 3 characters cannot be constructed" (isNothing $ mkTag "tg")

unit_tagTooLong :: IO ()
unit_tagTooLong = HU.assertBool "A tag that is longer than 20 characters cannot be constructed" (isNothing $ mkTag "ThisTagIsWayTooLongToBeValid")

unit_noControlCharInTag :: IO ()
unit_noControlCharInTag = HU.assertBool "A tag cannot contain control characters" (isNothing $ mkTag "Tag\^QWith\^PControl")

-- Quickspec test cases --

-- Ensure that a `Tag` is indeed constructed from an admissible string.
prop_validTag :: DI.ValidTag -> Bool
prop_validTag (DI.ValidTag tg) = DM.isJust $ mkTag tg

-- Ensure that construction properly validates the input string and returns a
-- `Tag` only if the input string is admissible.
prop_validation :: TU.Utf8Text1 -> Bool
prop_validation (TU.Utf8Text1 tg) = case mkTag tg of
  Just (Tag t) -> t == tg
  Nothing -> not $ isValidTag tg
  where
    isValidTag t =
      T.length t >= minTagLength
        && T.length t <= maxTagLength
        && T.all (\x -> TL1.isAlpha x || TL1.isWhiteSpace x) t
