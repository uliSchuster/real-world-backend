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
module Domain.Types
  ( mkEmail
  )
where

import           RIO
import qualified Text.Email.Validate           as Email

-- | Parse a text string into an RFC-compliant email-address.
mkEmail :: Text -> Maybe Email.EmailAddress
mkEmail = Email.emailAddress . encodeUtf8
