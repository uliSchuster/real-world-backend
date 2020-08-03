{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.TestUser where

import qualified Data.Maybe                    as DM
import qualified Domain.DomainInstances        as DI
import           Domain.User
import           RIO
import qualified RIO.Text                      as T
import           Test.QuickCheck.Instances.Text ( )
import qualified Test.Tasty.HUnit              as HU
import qualified Text.Email.Validate           as Email
import qualified Text.URI                      as URI

-- HUnit test cases --
unit_validUser :: IO ()
unit_validUser = HU.assertBool
  "Can a valid User be constructed?"
  (isJust $ mkUser "ulrich.schuster@koing.de"
                   "uliSchuster"
                   (Just "https://koing.de/expertise")
                   (Just "Nothing of interest")
  )

unit_validUserNoBio :: IO ()
unit_validUserNoBio = HU.assertBool
  "Can a valid User be constructed if there is no biography?"
  (isJust $ mkUser "tom@test.de" "tomTester" (Just "https://test.de") Nothing)

unit_validUserNoImageUrl :: IO ()
unit_validUserNoImageUrl = HU.assertBool
  "Can a valid User be constructed if there is no image link?"
  (isJust $ mkUser "ulrich.schuster@koing.de"
                   "uliSchuster"
                   Nothing
                   (Just "Nothing of interest")
  )

-- Quickspec test cases --
-- Ensure that a `User` can indeed b constructed from an admissible strings.
prop_validUser
  :: DI.ValidEmailAddress
  -> DI.ValidUsername
  -> Maybe DI.ValidURI
  -> Maybe Text
  -> Bool
prop_validUser (DI.ValidEmailAddress email) (DI.ValidUsername uName) mUri mBio
  = DM.isJust $ mkUser
    (T.decodeUtf8With T.lenientDecode $ Email.toByteString email)
    uName
    uri
    mBio
 where
  uri = case mUri of
    Nothing              -> Nothing
    Just (DI.ValidURI u) -> Just $ URI.render u
