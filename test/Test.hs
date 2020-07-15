{-# OPTIONS_GHC -F -pgmF tasty-discover #-}

-- This module serves as starting point for automatic test case discovery.
-- The above GHC options pragma, in combination with the "tasty-discovery"
-- package instructs GHC to automatically discover and execute all test cases 
-- in the current directory.
-- See https://www.stackage.org/package/tasty-discover
-- Depending on the test-case prefix, the corresponding test-runner is used:
-- unit_*: HUnit test case
-- prop_*: QuickCheck test case
-- test_*: Tasty test trees

-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

-- module Main where

-- import RIO
-- import qualified RIO.List as L
-- import Test.Tasty
-- import qualified Test.Tasty.HUnit as HU
-- import qualified Test.Tasty.QuickCheck as QC

-- main = defaultMain tests

-- tests :: TestTree
-- tests = testGroup "Tests" [qcProps, unitTests]

-- unitTests :: TestTree
-- unitTests =
--   testGroup
--     "Unit tests"
--     [ HU.testCase "List comparison (different length)" $
--         [1, 2, 3] `compare` [1, 2] HU.@?= GT,
--       -- the following test does not hold
--       HU.testCase "List comparison (same length)" $
--         [1, 2, 3] `compare` [1, 2, 2] HU.@?= LT
--     ]

-- qcProps :: TestTree
-- qcProps =
--   testGroup
--     "(checked by QuickCheck)"
--     [ QC.testProperty "sort == sort . reverse" $
--         \list -> L.sort (list :: [Int]) == L.sort (reverse list),
--       QC.testProperty "Fermat's little theorem" $
--         \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
--       -- the following property does not hold
--       QC.testProperty "Fermat's last theorem" $
--         \x y z n ->
--           (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
--     ]
