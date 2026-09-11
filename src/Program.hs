module Program (StepResult (..), RunResult (..), runProgram) where

import Ast
import qualified Data.Map as Map
import Diagnostic
import Evaluator
import Parser (parseStatement)
import Value (Value)

data StepResult = StepResult
  { output :: String
  , ast :: String
  , resultValue :: Maybe Value
  } deriving (Show)

data RunResult = RunResult
  { steps :: [StepResult]
  , diagnostic :: Maybe Diagnostic
  , finalEnvironment :: Env
  , runTrace :: TraceLog
  , runTraceTruncated :: Bool
  , runEvaluations :: Int
  } deriving (Show)

runProgram :: EvalOptions -> Env -> String -> RunResult
runProgram opts initial source = go initial initialEvalState [] (zip [1..] (lines source))
  where
    finish env state results err = RunResult (reverse results) err env (traceLog state)
      (traceTruncated state) (evaluationCount state)
    go env state results [] = finish env state results Nothing
    go env state results ((row, input) : rest) = case parseStatement row input of
      Left err -> finish env state results (Just err)
      Right Nothing -> go env state results rest
      Right (Just statement) ->
        let expression = case statement of Definition _ e -> e; Expression e -> e
            evaluated = case statement of
              Definition name e -> defineWithState opts env name e state
              Expression e -> evalWithState opts env e state
         in case evaluated of
              (Left err, nextState) -> finish env nextState results (Just err)
              (Right value, nextState) ->
                let nextEnv = case statement of Definition name _ -> Map.insert name value env; Expression _ -> env
                    displayed = case statement of Definition name _ -> "Defined: " ++ name; Expression _ -> show value
                    step = StepResult displayed (show (stripLocations expression)) (Just value)
                 in go nextEnv nextState (step : results) rest
