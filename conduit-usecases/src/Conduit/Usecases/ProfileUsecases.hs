{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  <File name or $Header$ to be replaced automatically>
-- Description :  Location where events can take place.
-- Copyright   :  (c) 2020, Real-World Haskell Study Group
-- License     :  Apache License 2.0
-- Maintainer  :  real-world-study-group@ugsmail.mailworks.org
-- Stability   :  unstable
-- Lang. Ext.  :  NoImplicitPrelude - Use RIO instead
--
-- Conduit use cases for displaying and following users.
-- Uses an abstract repository to access the persisted user profiles.
module Conduit.Usecases.ProfileUsecases
  ( getProfile
  , registerUser
  )
where

import qualified Data.Password.Argon2          as PW
import qualified Text.Password.Strength        as PWS
-- import           Data.Time.Clock                ( getCurrentTime
--                                                 , utctDay
--                                                 ) -- for pwd checking

import qualified Conduit.Domain.API            as D
import           RIO
import           RIO.Time                       ( utctDay
                                                , getCurrentTime
                                                )
import           Conduit.Usecases.UserRepositoryI


-- | Use the tag-repository configured at application-level to read all tags.
-- For those tags that fail to map on the domain type, log an error.
getProfile :: UserRepositoryI cfg => D.Username -> RIO cfg (Either Text D.User)
getProfile = readUser


registerUser
  :: UserRepositoryI cfg
  => D.User
  -> PW.Password
  -> RIO cfg (Either Text D.UserIdentity)
registerUser profile pwd = do
  refDay <- utctDay <$> liftIO getCurrentTime
  let pwdStrength =
        PWS.strength $ PWS.score PWS.en_US refDay (PW.unsafeShowPassword pwd)
  case pwdStrength of
    PWS.Risky ->
      return $ Left "Password is unsafe. Please choose a stronger password."
    PWS.Weak ->
      return $ Left "Passowrd is weak. Please choose a stronger passowrd."
    _ -> do
      pwdHash <- liftIO $ PW.hashPassword pwd
      return $ Right (D.UserIdentity pwdHash profile)
