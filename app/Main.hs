{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isSpace)
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Evaluator
import Network.Wai.Middleware.Cors (simpleCors)
import Parser
import System.Environment (getArgs)
import System.IO (hFlush, isEOF, stdout)
import Web.Scotty

-- ----------------------------------------------------------------------------
-- Web Server Specific Definitions
-- ----------------------------------------------------------------------------

data StepResult = StepResult {output :: String, ast :: Maybe String} deriving (Show)

instance ToJSON StepResult where
  toJSON (StepResult out maybeAst) = object ["output" .= out, "ast" .= maybeAst]

data MultiEvalResult = MultiEvalResult
  { steps :: [StepResult],
    finalError :: Maybe String,
    finalEnvironment :: Env,
    traceLog :: Maybe [String] -- Field for combined trace log
  }
  deriving (Show)

instance ToJSON MultiEvalResult where
  toJSON (MultiEvalResult stepList err envMap mLog) =
    object
      [ "steps" .= stepList,
        "finalError" .= err,
        "finalEnvironment" .= envMap,
        "traceLog" .= mLog
      ]

-- Timeout for evaluation in microseconds (e.g., 5 seconds)
evaluationTimeoutDuration :: Int
evaluationTimeoutDuration = 5 * 1000 * 1000 -- 5 seconds

-- Function to process a single line for the web handler's fold
-- Takes the environment accumulated so far, and the current line string
-- Returns Either Error Message (New Env, Output String, AST String, Trace Log for this line)
processWebLine :: Env -> String -> IO (Either String (Env, String, Maybe String, TraceLog))
processWebLine currentEnv line = do
  let trimmedLine = dropWhile isSpace line
  if null trimmedLine
    then return $ Right (currentEnv, "", Nothing, [])
    else case parse parseDefinition trimmedLine of
      Just ((name, defExpr), "") -> do
        let astString = Just (show defExpr)
        let envForDefEval = Map.insert name valueForDef currentEnv
            -- evalResultAndTrace now holds Either String (Value, TraceLog)
            evalResultAndTrace = eval envForDefEval defExpr
            valueForDef = case evalResultAndTrace of
              Left e -> error $ "Internal error: accessing value from failed recursive definition for " ++ name ++ ": " ++ e
              Right (v, _) -> v
        case evalResultAndTrace of
          Left err -> return $ Left $ "Error in definition '" ++ name ++ "': " ++ err
          Right (_, trace) -> return $ Right (envForDefEval, "Defined (rec): " ++ name, astString, trace)
      _ ->
        case parse parseExpr trimmedLine of
          Just (exprAST, "") -> do
            let astString = Just (show exprAST)
            case eval currentEnv exprAST of -- eval now returns Either String (Value, TraceLog)
              Left errMsg -> return $ Left errMsg
              Right (val, trace) -> return $ Right (currentEnv, show val, astString, trace)
          _ ->
            return $ Left $ "Parse Error on line: " ++ take 40 trimmedLine

-- Function to run the Scotty Web Server
runWebServer :: IO ()
runWebServer = do
  putStrLn "Starting L Language Web Server on port 3000 (Stateful, Multi-line, with Trace)..."
  sharedEnvRef <- newIORef initialEnv -- Initialize shared state with built-ins
  scotty 3000 $ do
    middleware simpleCors
    -- middleware logStdoutDev

    get "/" $ do
      file "static/index.html"

    get "/script.js" $ do
      setHeader "Content-Type" "application/javascript"
      file "static/script.js"

    -- Documentation Routes
    get "/docs/" $ file "docs/index.html"
    get "/docs" $ redirect "/docs/"
    get "/docs/:filename" $ do
      fileName <- param "filename"
      -- Simple directory traversal protection (very basic)
      -- Allow standard filenames and ensure no ".." traversal
      if not (".." `isInfixOf` fileName)
        then file $ "docs/" ++ fileName
        else next -- Pass through if invalid or let Scotty handle 404
    post "/evaluate" $ do
      codeText <- body
      let codeString = unpack (decodeUtf8 codeText)
      let codeLines = lines codeString

      initialEnvState <- liftIO $ readIORef sharedEnvRef

      -- Accumulator: Either Error (CurrentEnvInBlock, List of StepResults, Accumulated TraceLog)
      finalResult <-
        foldM
          ( \acc line -> case acc of
              Left err -> return $ Left err
              Right (env, stepsAcc, traceAcc) -> do
                lineResult <- liftIO $ processWebLine env line
                case lineResult of
                  Left err -> return $ Left err
                  Right (newEnv, outputStr, maybeAstStr, lineTrace) ->
                    let step = StepResult outputStr maybeAstStr
                     in return $ Right (newEnv, if null outputStr && isNothing maybeAstStr then stepsAcc else stepsAcc ++ [step], traceAcc ++ lineTrace)
          )
          (Right (initialEnvState, [], []))
          codeLines

      response <- case finalResult of
        Left errorMsg ->
          return $ MultiEvalResult [] (Just errorMsg) initialEnvState Nothing
        Right (finalEnvState, steps, accumulatedTrace) -> do
          liftIO $ atomicModifyIORef' sharedEnvRef $ const (finalEnvState, ())
          return $ MultiEvalResult steps Nothing finalEnvState (Just accumulatedTrace)

      json response

-- ----------------------------------------------------------------------------
-- REPL Specific Definitions
-- ----------------------------------------------------------------------------

data ReplState = ReplState
  { rsEnv :: Env,
    rsTrace :: Bool
  }

initialReplState :: ReplState
initialReplState = ReplState {rsEnv = initialEnv, rsTrace = True}

repl :: ReplState -> IO ()
repl state = do
  putStr "L-Repl> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn "\nGoodbye!"
    else do
      line <- getLine
      let trimmed = dropWhile isSpace line
      case words trimmed of
        (":quit" : _) -> putStrLn "Goodbye!"
        (":env" : _) -> do
          print (rsEnv state)
          repl state
        (":trace" : _) -> do
          let newTrace = not (rsTrace state)
          putStrLn $ "Trace mode is now " ++ if newTrace then "ON" else "OFF"
          repl state {rsTrace = newTrace}
        (":help" : _) -> do
          printHelp
          repl state
        (":examples" : _) -> do
          printExamples
          repl state
        (":load" : name : _) -> do
          case Map.lookup name examples of
            Just code -> do
              putStrLn $ "Loading example '" ++ name ++ "':\n" ++ code
              handleInput code state
            Nothing -> do
              putStrLn $ "Example '" ++ name ++ "' not found. Type :examples to see available ones."
              repl state
        _ -> handleInput line state

printHelp :: IO ()
printHelp = do
  putStrLn "Commands:"
  putStrLn "  :help          Show this help message"
  putStrLn "  :quit          Exit the REPL"
  putStrLn "  :env           Show current environment"
  putStrLn "  :trace         Toggle evaluation trace (default: ON)"
  putStrLn "  :examples      List available examples"
  putStrLn "  :load <name>   Load and run an example"
  putStrLn "\nSyntax:"
  putStrLn "  def = expr     Define a variable (supports recursion)"
  putStrLn "  expr           Evaluate an expression"

examples :: Map.Map String String
examples =
  Map.fromList
    [ ("factorial", "factorial = \\n -> if n == 0 then 1 else n * factorial (n - 1)"),
      ("fibonacci", "fib = \\n -> if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)"),
      ("map", "map = \\f xs -> if isEmpty xs then [] else cons (f (head xs)) (map f (tail xs))"),
      ("filter", "filter = \\p xs -> if isEmpty xs then [] else if p (head xs) then cons (head xs) (filter p (tail xs)) else filter p (tail xs)"),
      ("sumList", "sum = \\xs -> if isEmpty xs then 0 else head xs + sum (tail xs)"),
      ("length", "length = \\xs -> if isEmpty xs then 0 else 1 + length (tail xs)"),
      ("foldr", "foldr = \\f acc xs -> if isEmpty xs then acc else f (head xs) (foldr f acc (tail xs))"),
      ("reverse", "reverse = \\xs -> let append = \\a b -> if isEmpty a then b else cons (head a) (append (tail a) b) in if isEmpty xs then [] else append (reverse (tail xs)) (cons (head xs) [])")
    ]

printExamples :: IO ()
printExamples = do
  putStrLn "Available Examples:"
  mapM_ (\(name, _) -> putStrLn $ "  " ++ name) (Map.toList examples)

handleInput :: String -> ReplState -> IO ()
handleInput line state =
  let trimmedLine = dropWhile isSpace line
      currentEnv = rsEnv state
      showTrace = rsTrace state
   in if null trimmedLine
        then repl state
        else case parse parseDefinition trimmedLine of
          Just ((name, expr), "") -> do
            putStrLn $ "Evaluating potentially recursive definition for '" ++ name ++ "'..."
            let newEnv = Map.insert name value currentEnv
                evalResultAndTrace = eval newEnv expr -- Returns (Value, TraceLog)
                value = case evalResultAndTrace of
                  Left e -> Prelude.error $ "Internal error: accessing value from failed recursive eval for " ++ name ++ ": " ++ e
                  Right (v, _) -> v
            case evalResultAndTrace of
              Left err -> do
                putStrLn $ "Error in definition '" ++ name ++ "': " ++ err
                repl state
              Right (_, trace) -> do
                -- Definition succeeded
                putStrLn $ "Defined (rec): " ++ name
                if showTrace
                  then do
                    putStrLn "--- Evaluation Trace (Definition) ---"
                    mapM_ (putStrLn . ("  " ++)) trace
                    putStrLn "-------------------------------------"
                  else return ()
                repl state {rsEnv = newEnv}
          _ ->
            case parse parseExpr trimmedLine of
              Just (exprAST, "") -> do
                case eval currentEnv exprAST of -- Returns (Value, TraceLog)
                  Left err -> putStrLn $ "Error: " ++ err
                  Right (val, trace) -> do
                    -- Expression evaluation succeeded
                    print val
                    if showTrace
                      then do
                        putStrLn "--- Evaluation Trace ---"
                        mapM_ (putStrLn . ("  " ++)) trace
                        putStrLn "----------------------"
                      else return ()
                repl state
              Just (_, rest) -> do
                putStrLn ("Parse Error: Unexpected input near: '" ++ take 20 rest ++ "...'")
                repl state
              Nothing -> do
                putStrLn "Parse Error: Invalid input."
                repl state

runRepl :: IO ()
runRepl = do
  putStrLn "Starting L Language REPL..."
  putStrLn "Type :help for commands."
  repl initialReplState

-- ----------------------------------------------------------------------------
-- Main Entry Point - Selects Mode
-- ----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r"] -> runRepl
    ["-w"] -> runWebServer
    _ -> do
      putStrLn "Usage: <program-name> [-r | -w]"
      putStrLn "  -r: Run interactive REPL"
      putStrLn "  -w: Run web server on port 3000 (Stateful, Multi-line, with Trace)"
      putStrLn "Defaulting to REPL mode."
      runRepl
