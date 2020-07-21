{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.TestUsername where

import qualified Data.Maybe as DM
import Domain.Username
import qualified Domain.DomainInstances as DI
import RIO
import qualified RIO.Text as T
import qualified Test.Tasty.HUnit as HU
import qualified TestUtils as TU
import qualified Text.Latin1 as TL1

-- HUnit test cases --

unit_validUserName :: IO ()
unit_validUserName = HU.assertBool "Can a valid UserName be constructed?" (isJust $ mkUserName "uliSchuster")

unit_UserNameTooShort :: IO ()
unit_UserNameTooShort = HU.assertBool "A UserName that is less than 5 characters cannot be constructed" (isNothing $ mkUserName "min")

unit_UserNameTooLong :: IO ()
unit_UserNameTooLong = HU.assertBool "A UserName that longer than 20 characters cannot be constructed" (isNothing $ mkUserName "ThiIsAUSerNameThatIsTooLong")

unit_noWhitespaceInUserName :: IO ()
unit_noWhitespaceInUserName = HU.assertBool "A UserName that contains whitespace characters cannot be constructed" (isNothing $ mkUserName "Invalid Username")

unit_noControlCharInUserName :: IO ()
unit_noControlCharInUserName = HU.assertBool "A UserName that contains a control character cannot be constructed" (isNothing $ mkUserName "Invalid\^GUser")

unit_noNonAlphaInitial :: IO ()
unit_noNonAlphaInitial = HU.assertBool "A UserName that starts with a character that is not alphabetic cannot be constructed" (isNothing $ mkUserName "8UserName")

-- Quickspec test cases --

-- Ensure that a `UserName` is indeed constructed from an admissible string.
prop_validUserName :: DI.ValidUserName -> Bool
prop_validUserName (DI.ValidUserName uName) = DM.isJust $ mkUserName uName

-- Ensure that construction properly validates the input string and returns a
-- `UserName` only if the input string is admissible.
prop_validation :: TU.Utf8Text1 -> Bool
prop_validation (TU.Utf8Text1 t) = case mkUserName t of
  Just (UserName un) -> un == t
  Nothing -> not $ isValidUserName t
  where
    isValidUserName u =
      T.length u >= minUserNameLength
        && T.length u <= maxUserNameLength
        && T.all TL1.isPrintable u
        && TL1.isAlpha (T.index u 0)
