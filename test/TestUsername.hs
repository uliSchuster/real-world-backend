{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestUsername where

import qualified Data.Maybe as DM
import Domain.Username
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified Test.Tasty.HUnit as HU
import qualified TestUtils as TU

-- HUnit test cases --

unit_validUserName :: IO ()
unit_validUserName = HU.assertBool "Can a valid Username be constructed?" (isJust $ mkUserName "uliSchuster")

unit_UserNameTooShort :: IO ()
unit_UserNameTooShort = HU.assertBool "A UserName that is less than 5 characters cannot be constructed" (isNothing $ mkUserName "min")

unit_UserNameTooLong :: IO ()
unit_UserNameTooLong = HU.assertBool "A UserName that longer than 20 characters cannot be constructed" (isNothing $ mkUserName "ThiIsAUSerNameThatIsTooLong")

unit_noWhitespaceInUserName :: IO ()
unit_noWhitespaceInUserName = HU.assertBool "A UserName that contains whitespace characters cannot be constructed" (isNothing $ mkUserName "Invalid Username")

unit_noControlCharInUserName :: IO ()
unit_noControlCharInUserName = HU.assertBool "A UserName that contains a control character cannot be constructed" (isNothing $ mkUserName "Invalid\nUser")

unit_noNonAlphaInitial :: IO ()
unit_noNonAlphaInitial = HU.assertBool "A UserName that starts with a character that is not alphabetic cannot be constructed" (isNothing $ mkUserName "8UserName")

-- Quickspec test cases --
prop_validUserName :: TU.AlphaLatin1 -> TU.PrintableText -> Bool
prop_validUserName (TU.AlphaLatin1 a) (TU.PrintableText t)
  | T.length uName >= minUserNameLength && T.length uName <= maxUserNameLength = DM.isJust $ mkUserName uName
  | otherwise = DM.isNothing $ mkUserName uName
  where
    uName = T.cons a t -- First letter must be alpha
