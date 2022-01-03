module MicroTopLevel where 

import           LLVM.AST
import           LLVM.Pretty

import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T

import           System.IO
import           System.Directory
import           System.Process
import           System.Posix.Temp

import           Control.Exception              ( bracket )
