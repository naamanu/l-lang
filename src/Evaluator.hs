{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator
  ( eval,
    Env,
    initialEnv,
    TraceLog,
  )
where

import Ast
import qualified Data.Map as Map
import Parser
import Value

-- Define a type alias for the trace log
type TraceLog = [String]

-- Helper to indent trace messages for sub-evaluations
indentLog :: TraceLog -> TraceLog
indentLog = map ("  " ++)

-- ----------------------------------------------------------------------------
-- Placeholder Primitive Implementations & Initial Environment
-- ----------------------------------------------------------------------------

initialEnv :: Env
initialEnv =
  Map.empty

-- ----------------------------------------------------------------------------
-- Evaluator Core
-- ----------------------------------------------------------------------------

isVNum :: Value -> Bool
isVNum (VNum _) = True
isVNum _ = False

-- Helper for binary operations
evalBinaryOp ::
  Expr ->
  Expr ->
  Env ->
  String -> -- opSymbol (e.g., "+", "==")
  (Int -> Int -> Value) -> -- opArithFn: ONLY for arithmetic operations on Ints
  (Value -> Bool) -> -- typeCheckOperand: for validating operands (esp. for arithmetic)
  String -> -- expectedTypeStr: for error messages if typeCheckOperand fails
  Either String (Value, TraceLog)
evalBinaryOp e1 e2 env opSymbol opArithFn typeCheckOperand expectedTypeStr = do
  let entryLog = "Eval " ++ opSymbol ++ " on (" ++ show e1 ++ ") and (" ++ show e2 ++ ")"
  (v1, log1) <- eval env e1
  (v2, log2) <- eval env e2
  let opStepLog = " Performing " ++ opSymbol ++ " on " ++ show v1 ++ " and " ++ show v2

  if opSymbol == "=="
    then
      -- For Eq, type checking is trivial (handled by Value's Eq instance)
      -- The typeCheckOperand for Eq is (\_ -> True) because any pair is "valid" for Eq.
      let resVal = VBool (v1 == v2) -- Uses the 'Eq Value' instance
       in Right (resVal, [entryLog] ++ indentLog log1 ++ indentLog log2 ++ [opStepLog, "  -> " ++ show resVal])
    else
      -- For arithmetic operations (+, -, *)
      if typeCheckOperand v1 && typeCheckOperand v2
        then case (v1, v2) of
          (VNum n1, VNum n2) ->
            let resVal = opArithFn n1 n2 -- opArithFn is Int -> Int -> Value
             in Right (resVal, [entryLog] ++ indentLog log1 ++ indentLog log2 ++ [opStepLog, "  -> " ++ show resVal])
          _ -> Left ("Internal error: Type check passed but operands not VNum for arithmetic op '" ++ opSymbol ++ "'. Got " ++ show v1 ++ " and " ++ show v2)
        else Left ("Type Error: " ++ opSymbol ++ " expects " ++ expectedTypeStr ++ ". Got " ++ show v1 ++ " and " ++ show v2)

-- Main eval function
eval :: Env -> Expr -> Either String (Value, TraceLog)
eval env e = case e of
  Num n ->
    let v = VNum n
        trace = ["Eval Num " ++ show n ++ " -> " ++ show v]
     in Right (v, trace)
  BoolLit b ->
    let v = VBool b
        trace = ["Eval BoolLit " ++ show b ++ " -> " ++ show v]
     in Right (v, trace)
  Var x ->
    let entryLog = "Eval Var " ++ x
     in case Map.lookup x env of
          Just v -> Right (v, [entryLog ++ " -> " ++ show v])
          Nothing -> Left ("Undefined variable: " ++ x)
  Lam var body ->
    let v = VClosure var body env -- Crucially captures current env
        trace = ["Eval Lam " ++ var ++ " -> " ++ show v]
     in Right (v, trace)
  App fun arg -> do
    let entryLog = "Eval App (" ++ show fun ++ ") (" ++ show arg ++ ")"
    (funVal, funLog) <- eval env fun
    (argVal, argLog) <- eval env arg
    let applyLog = " Apply " ++ show funVal ++ " to " ++ show argVal
    case funVal of
      VClosure var body closureEnv -> do
        (resVal, bodyLog) <- eval (Map.insert var argVal closureEnv) body
        Right (resVal, [entryLog] ++ indentLog funLog ++ indentLog argLog ++ [applyLog] ++ indentLog bodyLog ++ ["  -> " ++ show resVal])
      VPrim name primFn ->
        case primFn argVal of -- Primitive itself doesn't return a log directly here
          Left err -> Left err
          Right resPrim -> Right (resPrim, [entryLog] ++ indentLog funLog ++ indentLog argLog ++ [applyLog, "  Primitive " ++ name ++ " -> " ++ show resPrim])
      _ -> Left ("Type Error: Attempted to apply non-function: " ++ show funVal)
  Let var boundExpr bodyExpr -> do
    let entryLog = "Eval Let " ++ var ++ " = " ++ show boundExpr ++ " in ..."
    (boundVal, boundLog) <- eval env boundExpr
    let env' = Map.insert var boundVal env
    let letStepLog = "  With " ++ var ++ " = " ++ show boundVal
    (bodyVal, bodyLog) <- eval env' bodyExpr
    Right (bodyVal, [entryLog] ++ indentLog boundLog ++ [letStepLog] ++ indentLog bodyLog ++ ["  -> " ++ show bodyVal])
  IfThenElse cond thn els -> do
    let entryLog = "Eval If " ++ show cond ++ " then ..."
    (condVal, condLog) <- eval env cond
    let stepCond = " Condition " ++ show cond ++ " evaluated to " ++ show condVal
    case condVal of
      VBool True -> do
        (thnVal, thnLog) <- eval env thn
        Right (thnVal, [entryLog] ++ indentLog condLog ++ [stepCond, "  Then branch taken"] ++ indentLog thnLog ++ ["  -> " ++ show thnVal])
      VBool False -> do
        (elsVal, elsLog) <- eval env els
        Right (elsVal, [entryLog] ++ indentLog condLog ++ [stepCond, "  Else branch taken"] ++ indentLog elsLog ++ ["  -> " ++ show elsVal])
      _ -> Left "Type Error: 'if' condition must evaluate to a Boolean."
  Add e1 e2 -> evalBinaryOp e1 e2 env "+" (\n1 n2 -> VNum (n1 + n2)) isVNum "numbers"
  Sub e1 e2 -> evalBinaryOp e1 e2 env "-" (\n1 n2 -> VNum (n1 - n2)) isVNum "numbers"
  Mul e1 e2 -> evalBinaryOp e1 e2 env "*" (\n1 n2 -> VNum (n1 * n2)) isVNum "numbers"
  Eq e1 e2 ->
    evalBinaryOp
      e1
      e2
      env
      "=="
      (\_n1 _n2 -> error "Arithmetic function should not be called for Eq") -- Dummy fn
      (\_val -> True) -- Type check for Eq operands: any value is fine
      "comparable values"
  List exprs -> do
    let entryLog = "Eval List [" ++ show (length exprs) ++ " elements]"
    -- Evaluate all elements and collect values and logs
    resultsAndLogs <- mapM (eval env) exprs
    let (vals, logsNested) = unzip resultsAndLogs
    let resVal = VList vals
    Right (resVal, [entryLog] ++ concatMap indentLog logsNested ++ ["  -> " ++ show resVal])
  Cons e1 e2 -> do
    let entryLog = "Eval Cons (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    (v1, log1) <- eval env e1
    (v2, log2) <- eval env e2
    let consStepLog = " Consing " ++ show v1 ++ " with " ++ show v2
    case v2 of
      VList vs ->
        let resVal = VList (v1 : vs)
         in Right (resVal, [entryLog] ++ indentLog log1 ++ indentLog log2 ++ [consStepLog, "  -> " ++ show resVal])
      _ -> Left "Type Error: Second argument to 'cons' must be a list."
  Head e -> do
    let entryLog = "Eval Head (" ++ show e ++ ")"
    (v, logSub) <- eval env e
    let headStepLog = " Head of " ++ show v
    case v of
      VList (x : _) -> Right (x, [entryLog] ++ indentLog logSub ++ [headStepLog, "  -> " ++ show x])
      VList [] -> Left "Runtime Error: Cannot take head of an empty list."
      _ -> Left "Type Error: 'head' requires a list argument."
  Tail e -> do
    let entryLog = "Eval Tail (" ++ show e ++ ")"
    (v, logSub) <- eval env e
    let tailStepLog = " Tail of " ++ show v
    case v of
      VList (_ : xs) ->
        let resVal = VList xs
         in Right (resVal, [entryLog] ++ indentLog logSub ++ [tailStepLog, "  -> " ++ show resVal])
      VList [] -> Left "Runtime Error: Cannot take tail of an empty list."
      _ -> Left "Type Error: 'tail' requires a list argument."
  IsEmpty e -> do
    let entryLog = "Eval IsEmpty (" ++ show e ++ ")"
    (v, logSub) <- eval env e
    let isEmptyStepLog = " IsEmpty on " ++ show v
    case v of
      VList [] -> Right (VBool True, [entryLog] ++ indentLog logSub ++ [isEmptyStepLog, "  -> True"])
      VList _ -> Right (VBool False, [entryLog] ++ indentLog logSub ++ [isEmptyStepLog, "  -> False"])
      _ -> Left "Type Error: 'isEmpty' requires a list argument."
