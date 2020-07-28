{-# LANGUAGE OverloadedStrings #-}
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
-- A command line interface (CLI) for the Conduit application. The CLI serves
-- as a simplified alternative to the ReST API. The goal is to offer a large
-- part of the application´s functionality also via CLI, to allow for
-- experimentation and simpler integration testing.
-- Command-line parsing uses the optparse-applicative package:
-- https://www.stackage.org/lts-16.7/package/optparse-applicative-0.15.1.0
module UI.CommandLine.CLI
  ( Command (..),
    ProfileCmd (..),
    parseCmdLine,
  )
where

import qualified Control.Error.Util as EUTL
import qualified Domain.Username as DUN
import qualified Options.Applicative as O
import RIO
import qualified RIO.Text as T

-- | All possible commands the user can input via the command line.
data Command
  = User
  | Profile ProfileCmd
  | Article
  | Comment
  | Tag
  deriving (Eq, Show)

newtype ProfileCmd
  = ShowProfile DUN.Username
  deriving (Eq, Show)

--- | FollowUser Text
--- | UnfollowUser Text

-- | Uses the `Options.Applicative`command line parser to determine
-- the user's instruction for the application what to do.
parseCmdLine :: IO Command
parseCmdLine = O.execParser cmdLineParserSpec

info' :: O.Parser a -> String -> O.ParserInfo a
info' p desc =
  O.info
    (O.helper <*> versionOption <*> p)
    ( O.fullDesc
        <> O.progDesc desc
        <> O.header
          "Conduit - A simple blogging application and a playground to learn Haskell programming for the real world."
    )

cmdLineParserSpec :: O.ParserInfo Command
cmdLineParserSpec = info' cmdLineParser "Blogging made too simple."
  where
    cmdLineParser :: O.Parser Command
    cmdLineParser =
      O.helper <*> versionOption
        <*> O.hsubparser
          ( O.command
              "user"
              ( O.info
                  userCmdParser
                  (O.progDesc "Register or login a user.")
              )
              <> O.command
                "profile"
                ( O.info
                    profileCmdParser
                    (O.progDesc "Inspect and modify a user profile.")
                )
              <> O.command
                "article"
                ( O.info
                    articleCmdParser
                    (O.progDesc "Publish, update and follow articles.")
                )
              <> O.command
                "comment"
                ( O.info
                    commentCmdParser
                    (O.progDesc "Create and update comments.")
                )
              <> O.command
                "tag"
                ( O.info
                    tagCmdParser
                    (O.progDesc "Inspect available tags.")
                )
          )

versionOption :: O.Parser (a -> a)
versionOption =
  O.infoOption "0.0.1.0" (O.long "version" <> O.help "Show version number.")

userCmdParser :: O.Parser Command
userCmdParser = undefined

-- | Parse the command line when the "profile" subcommand is given.
-- Currently, we take only one argument, the username.
profileCmdParser :: O.Parser Command
profileCmdParser =
  Profile
    <$> ( ShowProfile
            <$> O.argument usernameReader (O.metavar "<username>" <> O.help "Username of the profile to show.")
        )

articleCmdParser :: O.Parser Command
articleCmdParser = undefined

commentCmdParser :: O.Parser Command
commentCmdParser = undefined

-- | Parse the command line when the "tag" subcommand is given. Currently, there
-- are no options to parse here, we simply list all available tags.
tagCmdParser :: O.Parser Command
tagCmdParser = pure Tag

usernameReader :: O.ReadM DUN.Username
usernameReader = O.eitherReader $
  \s -> EUTL.note ("Not a valid username: " ++ s) (DUN.mkUsername $ T.pack s)
