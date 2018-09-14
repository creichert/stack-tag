
-- | `stack-tag' executable main module

module Main where

import Control.Applicative
import Data.Monoid
import Options.Applicative
import Stack.Tag

stackTagOptions :: Parser StackTagOpts
stackTagOptions =
   StackTagOpts
   <$> optional
      ( strOption
         ( long "stack-yaml"
        <> metavar "STACK_YAML"
        <> help "Location of stack.yaml" ) )
   <*> switch
        ( long "verbose"
        <> help "Verbose debug output" )
   <*> switch
        ( long "no-cache"
        <> help "Completely re-run tag command. Don't use cached tag files." )

optsDesc :: String
optsDesc = "Create ctags/etags for a project and all dependencies"

optsHeader :: String
optsHeader = "stack-tag - Create etags/ctags for a stack project based on snapshot"

stackTagOpts :: ParserInfo StackTagOpts
stackTagOpts =
  info
     (helper
      <*> infoOption "stack-tag v0.2.0, (c) Christopher Reichert 2018"
            ( long "version" <> help "Display version string" )
      <*> stackTagOptions )
     ( fullDesc
     <> progDesc optsDesc
     <> header optsHeader )

main :: IO ()
main = execParser stackTagOpts >>= stackTag
