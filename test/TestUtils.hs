{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestUtils
  ( Utf8Text1 (..),
    latin1Chars,
    PrintableLatin1 (..),
    PrintableText (..),
    alphaChars,
    AlphaLatin1 (..),
    alphaNumChars,
    AlphaNumLatin1 (..),
    AlphaNumText (..),
    AlphaNumSentence (..),
    punctuationChars,
    PunctuationLatin1,
  )
where

import RIO
import qualified RIO.Text as T
import Test.QuickCheck
import qualified Test.QuickCheck.Instances.List as QL
import qualified Test.QuickCheck.Utf8 as QCU

-- A nonempty UTF8 String
newtype Utf8Text1 = Utf8Text1 {getUtf8Text1 :: Text}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary Utf8Text1 where
  arbitrary = Utf8Text1 <$> QCU.genValidUtf81

-- A list of most (all?) printable Latin-1 characters.
latin1Chars :: String
latin1Chars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "\"!#$%&'()*+,-./:;<=>?[\\]^_`{|}~" <> "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"

-- Wrapper for a printable Latin-1 character, for use in QuickCheck properties.
-- The wrapper is necessary to define a separate `Arbitrary` instance.
newtype PrintableLatin1 = PrintableLatin1 {getPrintableLatin1 :: Char}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary PrintableLatin1 where
  -- Take one random element from the list of printable Latin-1 characters.
  arbitrary = PrintableLatin1 <$> elements latin1Chars

-- Wrapper for `Text`, to create an `Arbitrary` instance of a Text string that
-- consists of printable Latin-1 characters only.
newtype PrintableText = PrintableText {getPrintableText :: Text} deriving (Eq, Show, Ord, IsString, Display, Hashable, Semigroup, Monoid)

instance Arbitrary PrintableText where
  arbitrary = do
    sc <- listOf1 (arbitrary :: Gen PrintableLatin1) -- at least one character
    let ss = getPrintableLatin1 <$> sc -- unwrap
    return $ PrintableText (T.pack ss) -- convert to Text and wrap with Gen

-- A list of most (all?) alphabetic Latin-1 characters.
alphaChars :: String
alphaChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"

-- Wrapper for an alphabetic Latin-1 character, for use in QuickCheck
-- properties, again wrapped to create an `Arbitrary` instance.
newtype AlphaLatin1 = AlphaLatin1 {getAlphaLatin1 :: Char} deriving (Eq, Show, Ord, Display)

instance Arbitrary AlphaLatin1 where
  -- Take one random element from the list of alphabetic Latin-1 characters.
  arbitrary = AlphaLatin1 <$> elements alphaChars

-- A list of most (all?) alphanumeric Latin-1 characters.
alphaNumChars :: String
alphaNumChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"

-- Wrapper for an alphanumeric Latin-1 character, for use in QuickCheck
-- properties, again wrapped to create an `Arbitrary` instance.
newtype AlphaNumLatin1 = AlphaNumLatin1 {getAlphaNumLatin1 :: Char}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary AlphaNumLatin1 where
  -- Take one random element from the list of alphanumeric Latin-1 characters.
  arbitrary = AlphaNumLatin1 <$> elements alphaNumChars

-- Wrapper for `Text`, to create an `Arbitrary` instance of a Text string that
-- consists of alphanumeric Latin-1 characters only.
newtype AlphaNumText = AlphaNumText {getAlphaNumText :: Text} deriving (Eq, Show, Ord, IsString, Display, Hashable, Semigroup, Monoid)

instance Arbitrary AlphaNumText where
  arbitrary = do
    sc <- listOf1 (arbitrary :: Gen AlphaNumLatin1)
    let ss = getAlphaNumLatin1 <$> sc
    return $ AlphaNumText (T.pack ss)

-- Wrapper for a complete sentence with words taken from the alphanumeric
-- Latin-1 character set plus whitespace. No punctuation.
newtype AlphaNumSentence = AlphaNumSentence {getAlphaNumSentence :: Text} deriving (Eq, Show, Ord, IsString, Display, Hashable, Semigroup, Monoid)

instance Arbitrary AlphaNumSentence where
  arbitrary = do
    wc <- listOf1 (arbitrary :: Gen AlphaNumText)
    let ws = getAlphaNumText <$> wc
    return $ AlphaNumSentence (T.unwords ws)

-- A list of most (all?) Latin-1 punctuation characters.
punctuationChars :: String
punctuationChars = "\"!&(),-.:;?[]"

-- Wrapper for a Latin-1 punctuation character, for use in QuickCheck
-- properties, again wrapped to create an `Arbitrary` instance.
newtype PunctuationLatin1 = PunctuationLatin1 {getPunctuationLatin1 :: Char}
  deriving (Eq, Show, Ord, Display)

instance Arbitrary PunctuationLatin1 where
  -- Take one random element from the list of Latin-1 punctuation characters.
  arbitrary = PunctuationLatin1 <$> elements punctuationChars

-- Wrapper for a phrase - a sequence of sentences with punctuation characters
-- in between.
newtype Latin1Phrase = Latin1Phrase {getLatin1Phrase :: Text}
  deriving (Eq, Show, Ord, IsString, Display, Hashable, Semigroup, Monoid)

instance Arbitrary Latin1Phrase where
  arbitrary = do
    phrases <- listOf1 (arbitrary :: Gen AlphaNumSentence)
    let phraseList = getAlphaNumSentence <$> phrases
    punctuation <- QL.setLength (length phraseList) (arbitrary :: Gen PunctuationLatin1)
    let punctList = T.singleton . getPunctuationLatin1 <$> punctuation
    let phrase = merge phraseList punctList
    return $ Latin1Phrase (T.unwords phrase)

-- | Lazily merge two lists.
-- See: https://stackoverflow.com/a/3987188/4090111
merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x : xs) ys = x : merge ys xs
