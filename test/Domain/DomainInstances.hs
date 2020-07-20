{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.DomainInstances
  ( ValidUserName (..),
    ValidTitle (..),
  )
where

import Control.Monad
import qualified Domain.Username as UN
import RIO
import qualified RIO.Text as T
import Test.QuickCheck
import qualified Test.QuickCheck.Instances.List as QL
import qualified TestUtils as TU

-- Newtype wrapper to create an Arbitrary instance.
newtype ValidUserName = ValidUserName {getValidUserName :: Text}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary ValidUserName where
  arbitrary = do
    uNameSize <- choose (UN.minUserNameLength - 1, UN.maxUserNameLength - 1)
    ps <- replicateM uNameSize (arbitrary :: Gen TU.PrintableLatin1)
    firstLetter <- arbitrary :: Gen TU.AlphaLatin1
    let ss = TU.getPrintableLatin1 <$> ps -- unwrap
    let fl = TU.getAlphaLatin1 firstLetter
    return $ ValidUserName (T.pack (fl : ss)) -- convert to Text and wrap in Gen

-- Newtype wrapper to create an Arbitrary instance.
newtype ValidTitle = ValidTitle {getValidTitle :: Text}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary ValidTitle where
  arbitrary = do
    tws <- listOf1 (arbitrary :: Gen TU.AlphaNumText)
    let t = TU.getAlphaNumText <$> tws
    return $ ValidTitle (T.unwords t)
