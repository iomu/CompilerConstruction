import System.Environment (getArgs)
import System.Exit (exitFailure)

import ParCPP
import ErrM

import TypeChecker


-- driver

check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Left err -> do putStrLn "TYPE ERROR"
                                         putStrLn err
                                         exitFailure 
                          Right _    -> putStrLn "OK"

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: tccpp <SourceFile>"
                         exitFailure