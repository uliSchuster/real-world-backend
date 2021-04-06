{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Helper functions for domain types from third-party libraries
module Conduit.Domain.Types
  ( mkEmail
  , emailToText
  , uriToText
  )
where

import           RIO
import qualified RIO.Text as T
import qualified Text.Email.Validate           as Email
import qualified Text.URI                      as URI


-- | Parse a text string into an RFC-compliant email-address.
mkEmail :: Text -> Maybe Email.EmailAddress
mkEmail = Email.emailAddress . encodeUtf8

-- | Convert a valid email address to Text representation. Because the address 
-- is valid, there will be no UTF8 decoding error, so lenient decoding does not 
-- matter.
emailToText :: Email.EmailAddress -> Text
emailToText = T.decodeUtf8With T.lenientDecode . Email.toByteString

uriToText :: URI.URI -> Text
uriToText = URI.render