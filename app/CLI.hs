module CLI where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative

data Action
  = Ast
  | Sast
  | LLVM
  | Compile FilePath
  | Run
  deriving (Show, Eq)

data Options = Options {action :: Action, infile :: FilePath}
  deriving (Show, Eq)


actionP :: Parser Action
actionP =
  flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' Sast (long "sast" <> short 's' <> help "Pretty print the sast")
    <|> flag'
          LLVM
          (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag' Compile
              (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption (short 'o' <> value "a.out" <> metavar "FILE")
  -- running the file to see the expected output is default
    <|> pure Run


optionsP :: Parser Options
optionsP =
  Options
    <$> actionP
    <*> strArgument (help "Source file" <> metavar "FILE")
