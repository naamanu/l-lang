{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Ast (Expr (..))
import Control.Exception (evaluate)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?))
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import Diagnostic
import Evaluator
import Examples
import Program
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Test.HUnit
import qualified Test.QuickCheck as QC
import Value
import Web (evaluateResponse)

quiet :: EvalOptions
quiet = defaultEvalOptions {tracing = False}

run :: String -> RunResult
run = runProgram quiet initialEnv

lastValue :: RunResult -> Maybe Value
lastValue result = case reverse (steps result) of
  step : _ -> resultValue step
  [] -> Nothing

valueTest :: String -> Value -> Test
valueTest source expectedValue = TestLabel source $ TestCase $ do
  let result = run source
  assertEqual "no diagnostic" Nothing (diagnostic result)
  assertEqual "result" (Just expectedValue) (lastValue result)

errorTest :: String -> String -> Test
errorTest source expectedCode = TestLabel source $ TestCase $
  assertEqual "diagnostic code" (Just expectedCode) (code <$> diagnostic (run source))

baselineTests :: [Test]
baselineTests =
  [ valueTest "42" (VNum 42), valueTest "0" (VNum 0)
  , valueTest "0 - 5" (VNum (-5)), errorTest "-5" "parse.expected"
  , valueTest "True" (VBool True), valueTest "False" (VBool False)
  , valueTest "1 + 2" (VNum 3), valueTest "5 - 2" (VNum 3), valueTest "3 * 4" (VNum 12)
  , valueTest "1 + 2 * 3" (VNum 7), valueTest "(1 + 2) * 3" (VNum 9)
  , valueTest "10 - 3 - 2" (VNum 5)
  , valueTest "5 == 5" (VBool True), valueTest "5 == 3" (VBool False)
  , valueTest "True == True" (VBool True), valueTest "True == False" (VBool False)
  , valueTest "1 == False" (VBool False), valueTest "[] == []" (VBool True)
  , valueTest "[1,2] == [1,2]" (VBool True), valueTest "[1,2] == [1,3]" (VBool False)
  , valueTest "let x = 10 in x" (VNum 10), valueTest "let x = 5 in x + x" (VNum 10)
  , valueTest "let x = 3 in let y = 4 in x + y" (VNum 7)
  , valueTest "let x = 1 in let x = 2 in x" (VNum 2)
  , errorTest "let x = 5 in y" "name.undefined", errorTest "myVar" "name.undefined"
  , valueTest "if True then 100 else 200" (VNum 100)
  , valueTest "if False then 100 else 200" (VNum 200)
  , valueTest "if 1 == 1 then 1 else 0" (VNum 1)
  , valueTest "if 1 == 0 then 1 else 0" (VNum 0)
  , errorTest "if 1 then 10 else 20" "type.condition"
  , errorTest "1 + True" "type.arithmetic", errorTest "False + 2" "type.arithmetic"
  , errorTest "True * False" "type.arithmetic"
  , valueTest "[]" (VList []), valueTest "[1, 2, 3]" (VList [VNum 1,VNum 2,VNum 3])
  , valueTest "[1, True, []]" (VList [VNum 1,VBool True,VList []])
  , valueTest "cons 1 [2,3]" (VList [VNum 1,VNum 2,VNum 3])
  , valueTest "cons 1 []" (VList [VNum 1]), valueTest "head [1,2,3]" (VNum 1)
  , valueTest "tail [1,2,3]" (VList [VNum 2,VNum 3])
  , valueTest "isEmpty []" (VBool True), valueTest "isEmpty [1]" (VBool False)
  , errorTest "cons 1 2" "type.list", errorTest "head []" "runtime.empty-list"
  , errorTest "head 1" "type.list", errorTest "tail []" "runtime.empty-list"
  , errorTest "tail False" "type.list", errorTest "isEmpty 0" "type.list"
  , valueTest "let x = 10 in if x == 10 then (let y = x * 2 in y - 5) else 0" (VNum 15)
  , errorTest "(1 + 2" "parse.expected", errorTest "1 + #" "parse.expected"
  ]

languageTests :: [Test]
languageTests =
  [ valueTest "TrueValue = 7\nTrueValue" (VNum 7)
  , valueTest "letx = 8\nletx" (VNum 8)
  , valueTest "headway = 9\nheadway" (VNum 9)
  , errorTest "True = 1" "parse.expected", errorTest "let in = 2 in in" "parse.expected"
  , errorTest "\\if -> if" "parse.expected", errorTest "letx = 1 in x" "parse.expected"
  , valueTest "-- first line\nx = 3 -- definition\nx + 2 -- expression" (VNum 5)
  , valueTest "9223372036854775808 * 9223372036854775808" (VNum (9223372036854775808 ^ (2 :: Int)))
  , valueTest "(\\x -> x + 1) 4" (VNum 5)
  , valueTest "(λx -> x * 2) 4" (VNum 8)
  , TestCase $ assertEqual "UTF-16 source columns" (Just (Position 2 6))
      (start . sourceSpan <$> diagnostic (run "𝒙 = 1\n𝒙 + missing"))
  , valueTest "add = \\x y -> x + y\nadd 2 3" (VNum 5)
  , valueTest "add = \\x y -> x + y\ninc = add 1\ninc 4" (VNum 5)
  , valueTest "let x = 1 in let f = \\y -> x in let x = 2 in f 0" (VNum 1)
  , valueTest "x = 1\nf = \\y -> x\nx = 2\nf 0" (VNum 1)
  , valueTest "x = 1\nx = x + 1\nx" (VNum 2)
  , valueTest "f = \\x -> x + 1\ng = f\nf = \\x -> x + 2\ng 0" (VNum 1)
  , errorTest "x = x" "name.undefined", errorTest "x = 1 + x" "name.undefined"
  , errorTest "let f = \\x -> f x in f 1" "name.undefined"
  , errorTest "1 2" "type.application"
  , errorTest "(\\x -> 1) missing" "name.undefined"
  , valueTest "if True then 1 else missing" (VNum 1)
  , valueTest "if False then missing else 2" (VNum 2)
  , valueTest "f = \\x -> x\nf == f" (VBool False)
  , valueTest "[1] == [1,2]" (VBool False)
  , TestCase $ assertEqual "comma-separated output" "[1, 2]" (show (VList [VNum 1,VNum 2]))
  , TestCase $ do
      let result = run "x = 1\nx + 2\nx = missing\nx"
      assertEqual "successful outputs survive" ["Defined: x", "3"] (map output (steps result))
      assertEqual "failed definition does not replace x" (Just (VNum 1)) (Map.lookup "x" (finalEnvironment result))
      assertEqual "error location" (Just (Position 3 5)) (start . sourceSpan <$> diagnostic result)
  , TestCase $ do
      let result = run "x = 1\n1 + #"
      assertEqual "parse error keeps preceding statements" (Just (VNum 1)) (Map.lookup "x" (finalEnvironment result))
      assertEqual "furthest parse failure" (Just (Position 2 5)) (start . sourceSpan <$> diagnostic result)
  , TestCase $ do
      let first = run "x = 41"
          second = runProgram quiet (finalEnvironment first) "x + 1"
      assertEqual "explicit environment persists" (Just (VNum 42)) (lastValue second)
      assertEqual "fresh environment is isolated" (Just "name.undefined") (code <$> diagnostic (run "x"))
  ]

limitTests :: [Test]
limitTests =
  [ TestCase $ do
      let result = runProgram quiet {maxEvaluations = 30, maxDepth = 1000} initialEnv "loop = \\x -> loop x\nloop 0"
      assertEqual "step budget" (Just "limit.evaluations") (code <$> diagnostic result)
      assertEqual "exact step count" 30 (runEvaluations result)
  , TestCase $ do
      let result = runProgram quiet {maxDepth = 10} initialEnv "loop = \\x -> loop x\nloop 0"
      assertEqual "depth budget" (Just "limit.depth") (code <$> diagnostic result)
  , TestCase $ assertEqual "budget spans statements" (Just "limit.evaluations")
      (code <$> diagnostic (runProgram quiet {maxEvaluations = 2} initialEnv "1\n2\n3"))
  , TestCase $ do
      let result = runProgram defaultEvalOptions {maxTraceEntries = 3, maxTraceEntryLength = 20} initialEnv "123456789012345678901234567890 + 1"
      assertEqual "trace entry cap" 3 (length (runTrace result))
      assertBool "trace line cap" (all ((<= 20) . length) (runTrace result))
      assertBool "trace truncation flag" (runTraceTruncated result)
      assertEqual "truncation does not stop evaluation" Nothing (diagnostic result)
  , TestCase $ assertEqual "disabled trace" [] (runTrace (run "1 + 2"))
  , TestCase $ do
      let result = runProgram defaultEvalOptions initialEnv "1 + missing"
      assertBool "failed evaluation keeps trace" (not (null (runTrace result)))
  , errorTest (replicate 1100 '(' ++ "1" ++ replicate 1100 ')') "limit.parse-depth"
  ]

data Reply = Reply {replyError :: Maybe String, replyEnvironment :: Map.Map String String}
instance FromJSON Reply where
  parseJSON = withObject "Reply" $ \o -> Reply <$> o .:? "finalError" <*> o .: "finalEnvironment"

readReply :: Text.Text -> IO Reply
readReply payload = case eitherDecode (encodeUtf8 payload) of
  Left err -> assertFailure err >> fail err
  Right result -> pure result

webTests :: [Test]
webTests =
  [ TestCase $ do
      result <- evaluateResponse 5000000 quiet "large = 9223372036854775808\nf = \\x -> f x" >>= readReply
      assertEqual "exact JSON integer" (Just "9223372036854775808") (Map.lookup "large" (replyEnvironment result))
      assertEqual "recursive closure is opaque" (Just "<closure>") (Map.lookup "f" (replyEnvironment result))
  , TestCase $ do
      result <- evaluateResponse 0 quiet "1" >>= readReply
      assertBool "deadline emits a diagnostic" (maybe False (const True) (replyError result))
  , TestCase $ do
      completed <- timeout 2000000 $ do
        payload <- evaluateResponse 20000 quiet {maxEvaluations = maxBound, maxDepth = maxBound}
          "loop = \\x -> loop x\nloop 0"
        _ <- evaluate (Text.length payload)
        readReply payload
      case completed of
        Nothing -> assertFailure "Lazy evaluation escaped the server deadline"
        Just result -> assertBool "deadline covers evaluation and serialization"
          (maybe False (Text.isInfixOf "server deadline" . Text.pack) (replyError result))
  , TestCase $ do
      result <- evaluateResponse 1000000 quiet (replicate 65537 '1') >>= readReply
      assertBool "source limit" (maybe False (const True) (replyError result))
  , TestCase $ do
      _ <- evaluateResponse 5000000 quiet "x = 1"
      result <- evaluateResponse 5000000 quiet "x" >>= readReply
      assertBool "HTTP evaluation never persists definitions" (maybe False (const True) (replyError result))
  ]

propArithmetic :: QC.Small Integer -> QC.Small Integer -> QC.Small Integer -> Bool
propArithmetic (QC.Small a) (QC.Small b) (QC.Small c) =
  fmap fst (eval initialEnv (Add (Num a) (Mul (Num b) (Num c)))) == Right (VNum (a + b * c))

propWhitespace :: QC.NonNegative Int -> Bool
propWhitespace (QC.NonNegative n) = lastValue (run ("1" ++ spaces ++ "+" ++ spaces ++ "2")) == Just (VNum 3)
  where spaces = replicate (n `mod` 100) ' '

main :: IO ()
main = do
  args <- getArgs
  if args == ["--prove-failure"] then runTestTTAndExit (TestCase (assertFailure "intentional runner verification"))
  else do
    examples <- loadExamples
    let exampleTests = map (\e -> TestLabel (exampleId e) $ TestCase $ do
          let result = run (exampleCode e)
          assertEqual "example succeeds" Nothing (diagnostic result)
          assertEqual "example output" [exampleExpected e] (take 1 (reverse (map output (steps result))))) examples
    testCounts <- runTestTT (TestList (baselineTests ++ languageTests ++ limitTests ++ webTests ++ exampleTests))
    if errors testCounts + failures testCounts > 0 then exitFailure else pure ()
    properties <- mapM (QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = 200})
      [QC.property propArithmetic, QC.property propWhitespace]
    if all QC.isSuccess properties then pure () else exitFailure
