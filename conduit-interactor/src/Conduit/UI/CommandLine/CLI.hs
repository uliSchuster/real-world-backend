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
module Conduit.UI.CommandLine.CLI
  ( Command(..)
  , ProfileCmd(..)
  , ArticlesCmd(..)
  , parseCmdLine
  , parseStrings
  )
where

import qualified Control.Error.Util            as EUTL
import qualified Options.Applicative           as O
import qualified Conduit.Domain.API            as D
import qualified Conduit.Usecases.API          as U
import           RIO
import qualified RIO.Text                      as T

-- | All possible commands the user can input via the command line.
data Command
  = User
  | Profile ProfileCmd
  | Articles ArticlesCmd
  | Article D.Slug
  | Comment
  | Comments D.Slug
  | Tags
  deriving (Eq, Show)

newtype ProfileCmd
  = ShowProfile D.Username
  deriving (Eq, Show)

newtype ArticlesCmd
  = GetArticlesCmd U.ArticleQueryOptions
  deriving (Eq, Show)

--- | FollowUser Text
--- | UnfollowUser Text

-- | Uses the `Options.Applicative`command line parser to determine
-- the user's instruction for the application what to do.
parseCmdLine :: IO Command
parseCmdLine = O.execParser cmdLineParserSpec

-- | For testing the command line parser in apure setting.
parseStrings :: [String] -> O.ParserResult Command
parseStrings = O.execParserPure O.defaultPrefs cmdLineParserSpec

info' :: O.Parser a -> String -> O.ParserInfo a
info' p desc = O.info
  (O.helper <*> versionOption <*> p)
  (  O.fullDesc
  <> O.progDesc desc
  <> O.header
       "Conduit - A simple blogging application and a playground to learn Haskell programming for the real world."
  )

cmdLineParserSpec :: O.ParserInfo Command
cmdLineParserSpec = info' cmdLineParser "Blogging made too simple."
 where
  cmdLineParser :: O.Parser Command
  cmdLineParser = O.helper <*> versionOption <*> O.hsubparser
    (  O.command "user"
                 (O.info userCmdParser (O.progDesc "Register or login a user."))
    <> O.command
         "profile"
         (O.info profileCmdParser
                 (O.progDesc "Inspect and modify a user profile.")
         )
    <> O.command
         "articles"
         (O.info articlesCmdParser
                 (O.progDesc "Publish, update and follow articles.")
         )
    <> O.command
         "article"
         (O.info
           articleCmdParser
           (O.progDesc "Retrieve a specific article, identified by its slug.")
         )
    <> O.command
         "comment"
         (O.info commentCmdParser (O.progDesc "Create and update comments."))
    <> O.command
         "comments"
         (O.info
           commentsCmdParser
           (O.progDesc
             "Retrieve comments of a specific article, identified by its slug."
           )
         )
    <> O.command
         "tags"
         (O.info tagCmdParser (O.progDesc "Inspect available tags."))
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
    <$> (ShowProfile <$> O.argument
          usernameReader
          (O.metavar "<username>" <> O.help "Username of the profile to show.")
        )

articlesCmdParser :: O.Parser Command
articlesCmdParser =
  Articles
    <$> (   GetArticlesCmd
        <$> (   U.ArticleQueryOptions
            <$> O.option
                  O.auto
                  (  O.long "limit"
                  <> O.short 'l'
                  <> O.metavar "<limit>"
                  <> O.value 20
                  <> O.help "Maximum number of articles to show."
                  )
            <*> O.option
                  O.auto
                  (  O.long "offset"
                  <> O.short 'o'
                  <> O.metavar "<offset>"
                  <> O.value 0
                  <> O.help
                       "Offset from where on to show articles, ordered by date."
                  )
            <*> (   U.ArticleFilter
                <$> O.optional
                      (O.option
                        usernameReader
                        (  O.long "author"
                        <> O.short 'a'
                        <> O.metavar "<username>"
                        <> O.help
                             "Only show articles by the given author."
                        )
                      )
                <*> O.optional
                      (O.option
                        usernameReader
                        (  O.long "favoritedBy"
                        <> O.short 'f'
                        <> O.metavar "<username>"
                        <> O.help
                             "Only show articles favorited by the given user."
                        )
                      )
                <*> O.optional
                      (O.option
                        tagReader
                        (  O.long "tag"
                        <> O.short 't'
                        <> O.metavar "<tagname>"
                        <> O.help
                             "Only show articles tagged with the given tag."
                        )
                      )
                )
            )
        )


articleCmdParser :: O.Parser Command
articleCmdParser = Article <$> O.argument
  slugReader
  (O.metavar "<slug>" <> O.help "Slug of the article to show.")

commentCmdParser :: O.Parser Command
commentCmdParser = undefined

commentsCmdParser :: O.Parser Command
commentsCmdParser = Comments <$> O.argument
  slugReader
  (O.metavar "<slug>" <> O.help "Slug of the commented article.")

-- | Parse the command line when the "tag" subcommand is given. Currently, there
-- are no options to parse here, we simply list all available tags.
tagCmdParser :: O.Parser Command
tagCmdParser = pure Tags

usernameReader :: O.ReadM D.Username
usernameReader = O.eitherReader
  $ \s -> EUTL.note ("Not a valid username: " ++ s) (D.mkUsername $ T.pack s)

tagReader :: O.ReadM D.Tag
tagReader = O.eitherReader
  $ \s -> EUTL.note ("Not a valid tagname: " ++ s) (D.mkTag $ T.pack s)

slugReader :: O.ReadM D.Slug
slugReader = O.eitherReader
  $ \s -> EUTL.note ("Not a valid slug: " ++ s) (D.mkSlugFromText $ T.pack s)
