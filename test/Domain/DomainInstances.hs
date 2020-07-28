{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.DomainInstances
  ( ValidUsername (..),
    ValidTitle (..),
    ValidTag (..),
  )
where

import Control.Monad
import qualified Data.Char as C
import qualified Domain.Tag as DT
import qualified Domain.Username as DUN
import RIO
import qualified RIO.Text as T
import Test.QuickCheck
import qualified Test.QuickCheck.Instances.List as QL
import qualified TestUtils as TU

-- Newtype wrapper to create an Arbitrary instance.
newtype ValidUsername = ValidUsername {getValidUsername :: Text}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary ValidUsername where
  arbitrary = do
    uNameSize <- choose (DUN.minUsernameLength - 1, DUN.maxUsernameLength - 1)
    ps <- replicateM uNameSize (arbitrary :: Gen TU.PrintableLatin1)
    firstLetter <- arbitrary :: Gen TU.AlphaLatin1
    let ss = TU.getPrintableLatin1 <$> ps -- unwrap
    let fl = TU.getAlphaLatin1 firstLetter
    return $ ValidUsername (T.pack (fl : ss)) -- convert to Text and wrap in Gen

-- Newtype wrapper to create an Arbitrary instance.
newtype ValidTitle = ValidTitle {getValidTitle :: Text}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary ValidTitle where
  arbitrary = do
    tws <- listOf1 (arbitrary :: Gen TU.AlphaNumText)
    let t = TU.getAlphaNumText <$> tws
    return $ ValidTitle (T.unwords t)

-- Newtype wrapper to create an Arbitrary instance.
newtype ValidTag = ValidTag {getValidTag :: Text}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary ValidTag where
  arbitrary = do
    tagSize <- choose (DT.minTagLength - 2, DT.maxTagLength - 2)
    firstLetter <- TU.getAlphaLatin1 <$> (arbitrary :: Gen TU.AlphaLatin1)
    lastLetter <- TU.getAlphaLatin1 <$> (arbitrary :: Gen TU.AlphaLatin1)
    ts <- replicateM tagSize (frequency [(10, TU.getAlphaLatin1 <$> (arbitrary :: Gen TU.AlphaLatin1)), (1, elements [C.chr 32, C.chr 160])])
    return $ ValidTag (T.pack $ (firstLetter : ts) ++ [lastLetter])
