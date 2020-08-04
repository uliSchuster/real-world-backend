{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.TestTitle where

import           Data.Maybe
import qualified Domain.DomainInstances        as DI
import           Domain.Title
import           Domain.ValidationUtils         ( toCanonicText )
import           RIO
import qualified RIO.Text                      as T
import qualified Test.Tasty.HUnit              as HU
import qualified TestUtils                     as TU
import qualified Text.Latin1                   as TL1

-- HUnit test cases -

unit_validTitle :: IO ()
unit_validTitle = HU.assertBool
  "Can a valid Title be constructed?"
  (isJust $ mkTitle "The Title of my Blog Post")

unit_canonicalTitle :: IO ()
unit_canonicalTitle = HU.assertEqual
  "Is a title string converted to canonical form?"
  (getTitle . fromJust . mkTitle $ " A  not Canonical   Title   ")
  "A not Canonical Title"

-- Quickspec test cases --
prop_validTitle :: DI.ValidTitle -> Bool
prop_validTitle (DI.ValidTitle vt) = isJust $ mkTitle vt

prop_validation :: TU.Utf8Text1 -> Bool
prop_validation (TU.Utf8Text1 t) = case mkTitle t of
  Just (Title vt) -> vt == toCanonicText t
  Nothing         -> not $ isValidTitle t
 where
  isValidTitle tt =
    not (T.null tt) && T.all (\x -> TL1.isAlphaNum x || TL1.isWhiteSpace x) tt
