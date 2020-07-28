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
-- Clean-Architecture-Style dependency inversion: The inner use case ring
-- should not depend on the implementation of the persistence mechanism.
-- Therefore, the persistence interface is defined here in the form of a type -- class.
module Usecases.UserRepositoryI
  ( UserRepositoryI (),
    readUser,
  )
where

import qualified Domain.User as DU
import qualified Domain.Username as DUN
import RIO

-- | Interface of the actual persistence engine employed by this use case.
-- Must be implemented by the outermost application ring.
class UserRepositoryI userRepo where
  readUser :: DUN.Username -> RIO userRepo (Either Text DU.User)
