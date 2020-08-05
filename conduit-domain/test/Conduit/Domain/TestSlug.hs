{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Conduit.Domain.TestSlug where

import qualified Data.Maybe                    as DM
import qualified Test.Tasty.HUnit              as HU
import qualified Conduit.Domain.DomainInstances
                                               as DI
import           Conduit.Domain.Title
import           RIO

-- HUnit test cases --

unit_validSlug :: IO ()
unit_validSlug = HU.assertBool
  "Can a valid slug be constructed from a string?"
  (isJust $ mkSlugFromText "this-is-Some-test-Slug")

unit_validSlugNum :: IO ()
unit_validSlugNum = HU.assertBool
  "Can a valid slug with numbers be constructed from a string?"
  (isJust $ mkSlugFromText "this-is-1-test-Slug-123")

unit_validSlugSpace :: IO ()
unit_validSlugSpace = HU.assertBool
  "Can a valid slug with leading and trailing space be constructed from a string?"
  (isJust $ mkSlugFromText "  this-is-1-test-Slug-123 ")

unit_invalidSlugPunctuation :: IO ()
unit_invalidSlugPunctuation = HU.assertBool
  "Do punctuation characters result in slug creation failure?"
  (isNothing $ mkSlugFromText ":this-slug-is-invalid")


-- Quickspec test cases --

-- Ensure that a `Slug` can indeed be constructed from an admissible title.
prop_validSlug :: DI.ValidTitle -> Bool
prop_validSlug (DI.ValidTitle ti) = DM.isJust $ mkSlugFromText s
  where (Slug s) = mkSlug . DM.fromJust $ mkTitle ti
