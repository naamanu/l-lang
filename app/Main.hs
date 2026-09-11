module Main (main) where

import Data.List (find)
import qualified Data.Map as Map
import Diagnostic (renderDiagnostic)
import Evaluator (Env)
import Examples
import Lib
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)
import Web (runWebServer)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> startRepl
    ["-r"] -> startRepl
    ["-w"] -> do
      configuredPort <- lookupEnv "PORT"
      assets <- maybe "web-client/dist" id <$> lookupEnv "L_LANG_ASSETS"
      case maybe (Just 3000) readMaybe configuredPort of
        Just port | port > 0 && port < 65536 -> runWebServer port assets
        _ -> die "PORT must be an integer between 1 and 65535"
    _ -> die "Usage: l-lang-exe [-r | -w]"

startRepl :: IO ()
startRepl = do
  examples <- loadExamples
  putStrLn "L REPL — :help for commands"
  repl examples initialEnv defaultEvalOptions

repl :: [Example] -> Env -> EvalOptions -> IO ()
repl examples env opts = do
  putStr "L-Repl> "
  hFlush stdout
  eof <- isEOF
  if eof then putStrLn "Goodbye!" else do
    input <- getLine
    case words input of
      [":quit"] -> putStrLn "Goodbye!"
      [":help"] -> help >> again
      [":?"] -> help >> again
      [":env"] -> mapM_ (\(name, value) -> putStrLn (name ++ " = " ++ show value)) (Map.toList env) >> again
      [":trace"] -> do
        let enabled = not (tracing opts)
        putStrLn ("Trace " ++ if enabled then "ON" else "OFF")
        repl examples env opts {tracing = enabled}
      [":examples"] -> mapM_ (\e -> putStrLn (exampleId e ++ " — " ++ exampleTitle e)) examples >> again
      [":load", name] -> case find ((== name) . exampleId) examples of
        Nothing -> putStrLn ("Unknown example: " ++ name) >> again
        Just example -> execute (exampleCode example)
      _ -> execute input
  where
    again = repl examples env opts
    execute source = do
      let result = runProgram opts env source
      mapM_ (putStrLn . output) (steps result)
      mapM_ putStrLn (runTrace result)
      if runTraceTruncated result then putStrLn "Trace truncated." else pure ()
      maybe (pure ()) (putStrLn . renderDiagnostic) (diagnostic result)
      repl examples (finalEnvironment result) opts

help :: IO ()
help = putStrLn $ unlines
  [ ":quit                 Exit"
  , ":env                  Display definitions"
  , ":trace                Toggle bounded evaluation tracing"
  , ":examples             List executable examples"
  , ":load <name>          Run an example in the current environment"
  , "name = expression     Define a value; named lambdas may recurse"
  , "expression            Evaluate an expression"
  ]
