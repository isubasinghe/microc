{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CLI as C
import qualified Data.Text.IO as TIO
import qualified MicroAST as A
import MicroParser (programP)
import qualified MicroParser as P
import qualified MicroSemant as MS
import qualified MicroTopLevel as TL
import qualified MicroCodegen as CG
import qualified Options.Applicative as OP
import LLVM.Pretty (ppllvm)
import Text.Megaparsec (errorBundlePretty, runParser)
import Data.String.Conversions (cs)

main :: IO ()
main = runOpts =<< OP.execParser (C.optionsP `withInfo` infoString)
  where
    withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc
    infoString = "Run the mcc compiler on a given file"

processAction :: C.Action -> A.Program -> IO ()
processAction act ast = case act of
  C.Ast -> print ast
  _ -> case MS.checkProgram ast of
    Left err -> print err
    Right sast ->
      let llvm = CG.codegenProgram sast
       in case act of
            C.Sast -> print sast
            C.LLVM -> TIO.putStrLn . cs . ppllvm $ llvm
            C.Compile outfile -> undefined
            _ -> error $ show act 

runOpts :: C.Options -> IO ()
runOpts (C.Options action infile) = do
  programSource <- TIO.readFile infile
  let parseTree = runParser programP infile programSource
  case parseTree of
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> processAction action ast
