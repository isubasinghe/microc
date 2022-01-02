{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Options.Applicative as OP 
import qualified MicroAST as A
import qualified MicroParser as P 
import qualified Data.Text.IO as TIO
import qualified MicroSemant as MS 
import qualified CLI as C
import Text.Megaparsec (runParser, errorBundlePretty)
import MicroParser (programP)

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
                                 Right sast -> print sast

runOpts :: C.Options -> IO ()
runOpts (C.Options action infile) = do 
  programSource <- TIO.readFile infile 
  let parseTree = runParser programP infile programSource
  case parseTree of 
    Left err -> putStrLn $ errorBundlePretty err
    Right ast -> processAction action ast

