{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestUsername where

import Domain.Username
import RIO
import qualified RIO.List as L
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

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
unit_noNonAlphaInitial = HU.assertBool "A UserName that starts with a character that is not alphanumeric cannot be constructed" (isNothing $ mkUserName "8UserName")

-- Quickspec test cases --
prop_example :: [Int] -> Bool
prop_example list = L.sort (list :: [Int]) == L.sort (reverse list)
