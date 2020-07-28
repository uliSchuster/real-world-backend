{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.TestUsername where

import qualified Data.Maybe as DM
import qualified Domain.DomainInstances as DI
import Domain.Username
import RIO
import qualified RIO.Text as T
import qualified Test.Tasty.HUnit as HU
import qualified TestUtils as TU
import qualified Text.Latin1 as TL1

-- HUnit test cases --

unit_validUsername :: IO ()
unit_validUsername = HU.assertBool "Can a valid UserName be constructed?" (isJust $ mkUsername "uliSchuster")

unit_UsernameTooShort :: IO ()
unit_UsernameTooShort = HU.assertBool "A UserName that is less than 5 characters cannot be constructed" (isNothing $ mkUsername "min")

unit_UsernameTooLong :: IO ()
unit_UsernameTooLong = HU.assertBool "A Username that longer than 20 characters cannot be constructed" (isNothing $ mkUsername "ThiIsAUsernameThatIsTooLong")

unit_noWhitespaceInUsername :: IO ()
unit_noWhitespaceInUsername = HU.assertBool "A Username that contains whitespace characters cannot be constructed" (isNothing $ mkUsername "Invalid Username")

unit_noControlCharInUsername :: IO ()
unit_noControlCharInUsername = HU.assertBool "A Username that contains a control character cannot be constructed" (isNothing $ mkUsername "Invalid\^GUser")

unit_noNonAlphaInitial :: IO ()
unit_noNonAlphaInitial = HU.assertBool "A Username that starts with a character that is not alphabetic cannot be constructed" (isNothing $ mkUsername "8UserName")

-- Quickspec test cases --

-- Ensure that a `UserName` is indeed constructed from an admissible string.
prop_validUsername :: DI.ValidUsername -> Bool
prop_validUsername (DI.ValidUsername uName) = DM.isJust $ mkUsername uName

-- Ensure that construction properly validates the input string and returns a
-- `UserName` only if the input string is admissible.
prop_validation :: TU.Utf8Text1 -> Bool
prop_validation (TU.Utf8Text1 t) = case mkUsername t of
  Just (Username un) -> un == t
  Nothing -> not $ isValidUsername t
  where
    isValidUsername u =
      T.length u >= minUsernameLength
        && T.length u <= maxUsernameLength
        && T.all TL1.isPrintable u
        && TL1.isAlpha (T.index u 0)
