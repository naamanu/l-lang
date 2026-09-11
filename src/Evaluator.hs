{-# LANGUAGE BangPatterns #-}
module Evaluator
  ( eval, evalWithState, defineWithState, Env, initialEnv, TraceLog
  , EvalOptions (..), defaultEvalOptions, EvalState, initialEvalState
  , evaluationCount, traceLog, traceTruncated
  ) where

import Ast
import Control.Monad (ap)
import qualified Data.Map as Map
import Diagnostic
import Value

type TraceLog = [String]

data EvalOptions = EvalOptions
  { maxEvaluations :: Int
  , maxDepth :: Int
  , tracing :: Bool
  , maxTraceEntries :: Int
  , maxTraceEntryLength :: Int
  } deriving (Eq, Show)

defaultEvalOptions :: EvalOptions
defaultEvalOptions = EvalOptions 100000 1000 True 2000 512

data EvalState = EvalState
  { evaluationCount :: !Int
  , traceCount :: !Int
  , reversedTrace :: TraceLog
  , traceTruncated :: !Bool
  }

initialEvalState :: EvalState
initialEvalState = EvalState 0 0 [] False

traceLog :: EvalState -> TraceLog
traceLog = reverse . reversedTrace

initialEnv :: Env
initialEnv = Map.empty

-- State is retained on errors so a failed run can explain what happened.
newtype Evaluation a = Evaluation {runEvaluation :: EvalState -> (Either Diagnostic a, EvalState)}
instance Functor Evaluation where
  fmap f action = action >>= pure . f
instance Applicative Evaluation where
  pure a = Evaluation (\s -> (Right a, s))
  (<*>) = ap
instance Monad Evaluation where
  action >>= next = Evaluation $ \s -> case runEvaluation action s of
    (Left d, s') -> (Left d, s')
    (Right a, s') -> runEvaluation (next a) s'

failure :: SourceSpan -> String -> String -> Evaluation a
failure loc errorCode reason = Evaluation (\s -> (Left (Diagnostic errorCode reason loc), s))

record :: EvalOptions -> String -> Evaluation ()
record opts entry = Evaluation $ \s ->
  if not (tracing opts) then (Right (), s)
  else if traceCount s >= maxTraceEntries opts then (Right (), s {traceTruncated = True})
  else
    let limit = max 1 (maxTraceEntryLength opts)
        prefix = take (limit + 1) entry
        shortened = length prefix > limit
        displayed = if shortened then take (limit - 1) prefix ++ "…" else prefix
     in (Right (), s {traceCount = traceCount s + 1, reversedTrace = displayed : reversedTrace s,
                      traceTruncated = traceTruncated s || shortened})

tick :: EvalOptions -> Int -> SourceSpan -> Evaluation ()
tick opts depth loc = Evaluation $ \s ->
  if evaluationCount s >= maxEvaluations opts then
    (Left (Diagnostic "limit.evaluations" "Evaluation step limit exceeded" loc), s)
  else if depth >= maxDepth opts then
    (Left (Diagnostic "limit.depth" "Evaluation nesting limit exceeded" loc), s)
  else (Right (), s {evaluationCount = evaluationCount s + 1})

evaluateExpr :: EvalOptions -> Int -> SourceSpan -> Env -> Expr -> Evaluation Value
evaluateExpr opts depth inherited env expression = case expression of
  At loc e -> evaluateExpr opts depth loc env e
  _ -> do
    tick opts depth inherited
    record opts (replicate (min 40 depth) ' ' ++ "Eval " ++ show (stripLocations expression))
    value <- go expression
    record opts (replicate (min 40 depth) ' ' ++ "=> " ++ show value)
    pure value
  where
    sub = evaluateExpr opts (depth + 1) inherited env
    err = failure inherited
    binary operation left right = do
      a <- sub left
      b <- sub right
      case (a, b) of
        (VNum x, VNum y) -> let !n = operation x y in pure (VNum n)
        _ -> err "type.arithmetic" "Arithmetic expects two integers"
    go expr = case expr of
      Num n -> n `seq` pure (VNum n)
      BoolLit b -> pure (VBool b)
      Var name -> case Map.lookup name env of
        Just value -> pure value
        Nothing -> err "name.undefined" ("Undefined variable: " ++ name)
      Lam name body -> pure (VClosure name body env)
      App function argument -> do
        f <- sub function
        value <- sub argument
        case f of
          VClosure name body captured -> evaluateExpr opts (depth + 1) inherited (Map.insert name value captured) body
          _ -> err "type.application" ("Cannot apply a non-function: " ++ take 100 (show f))
      Let name bound body -> do
        value <- sub bound
        evaluateExpr opts (depth + 1) inherited (Map.insert name value env) body
      IfThenElse condition yes no -> do
        value <- sub condition
        case value of
          VBool True -> sub yes
          VBool False -> sub no
          _ -> err "type.condition" "The if condition must be a Boolean"
      Add a b -> binary (+) a b
      Sub a b -> binary (-) a b
      Mul a b -> binary (*) a b
      Eq a b -> do
        x <- sub a
        y <- sub b
        let !same = equalValues x y
        pure (VBool same)
      List elements -> VList <$> mapM sub elements
      Cons item rest -> do
        value <- sub item
        list <- sub rest
        case list of
          VList values -> pure (VList (value : values))
          _ -> err "type.list" "The second argument to cons must be a list"
      Head list -> do
        value <- sub list
        case value of
          VList (first : _) -> pure first
          VList [] -> err "runtime.empty-list" "Cannot take head of an empty list"
          _ -> err "type.list" "head expects a list"
      Tail list -> do
        value <- sub list
        case value of
          VList (_ : rest) -> pure (VList rest)
          VList [] -> err "runtime.empty-list" "Cannot take tail of an empty list"
          _ -> err "type.list" "tail expects a list"
      IsEmpty list -> do
        value <- sub list
        case value of
          VList values -> pure (VBool (null values))
          _ -> err "type.list" "isEmpty expects a list"
      At loc inner -> evaluateExpr opts depth loc env inner

evalWithState :: EvalOptions -> Env -> Expr -> EvalState -> (Either Diagnostic Value, EvalState)
evalWithState opts env expr = runEvaluation (evaluateExpr opts 0 (pointSpan 1 1) env expr)

-- Only syntactic lambdas get a recursive environment. Their captured map stays
-- lazy; no arbitrary expression is tied into a self-referential value thunk.
defineWithState :: EvalOptions -> Env -> String -> Expr -> EvalState -> (Either Diagnostic Value, EvalState)
defineWithState opts env name expr = evalWithState opts definitionEnv expr
  where
    definitionEnv = case lambda expr of
      Just (argument, body) -> let captured = Map.insert name (VClosure argument body captured) env in captured
      Nothing -> env
    lambda (At _ e) = lambda e
    lambda (Lam argument body) = Just (argument, body)
    lambda _ = Nothing

eval :: Env -> Expr -> Either String (Value, TraceLog)
eval env expr = case evalWithState defaultEvalOptions env expr initialEvalState of
  (Left diagnostic, _) -> Left (message diagnostic)
  (Right value, state) -> Right (value, traceLog state)
