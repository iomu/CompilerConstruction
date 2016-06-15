module Main where

import Codegen
import Emit

{-import Control.Monad.Trans-}

import System.IO
import System.Environment
{-import System.Console.Haskeline-}

import qualified LLVM.General.AST as AST

import System.Environment (getArgs)
import System.Exit (exitFailure)

import ParCPP
import ErrM
import PrintCPP
import TypeChecker


-- driver

check :: String -> IO (Maybe AST.Module) 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Left err -> do putStrLn "TYPE ERROR"
                                         putStrLn err
                                         exitFailure 
                          Right x    -> do
                            putStrLn $ printTree x
                            ast <- codegen initModule x
                            return $ Just ast

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check >> return ()
            _      -> do putStrLn "Usage: tccpp <SourceFile>"
                         exitFailure

initModule :: AST.Module
initModule = emptyModule "my cool codegen"


