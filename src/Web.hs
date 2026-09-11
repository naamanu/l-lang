{-# LANGUAGE OverloadedStrings #-}
module Web (runWebServer, evaluateResponse, responseJSON) where

import Control.Exception (evaluate)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeUtf8, decodeUtf8')
import Diagnostic
import Evaluator
import Program
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Timeout (timeout)
import qualified Value as Runtime
import Web.Scotty

valueJSON :: Runtime.Value -> Value
valueJSON value = case value of
  Runtime.VNum n -> toJSON (show n)
  Runtime.VBool b -> toJSON b
  Runtime.VList values -> toJSON (map valueJSON values)
  Runtime.VClosure {} -> toJSON ("<closure>" :: String)

diagnosticJSON :: Diagnostic -> Value
diagnosticJSON d = object
  [ "code" .= code d, "message" .= message d
  , "span" .= object ["start" .= pos (start (sourceSpan d)), "end" .= pos (end (sourceSpan d))]
  ]
  where pos p = object ["line" .= line p, "column" .= column p]

responseJSON :: RunResult -> Value
responseJSON result = object
  [ "steps" .= map stepJSON (steps result)
  , "finalError" .= fmap renderDiagnostic (diagnostic result)
  , "diagnostic" .= fmap diagnosticJSON (diagnostic result)
  , "finalEnvironment" .= Map.map valueJSON (finalEnvironment result)
  , "traceLog" .= runTrace result
  , "traceTruncated" .= runTraceTruncated result
  , "evaluations" .= runEvaluations result
  ]
  where stepJSON step = object ["output" .= output step, "ast" .= ast step]

errorResponse :: String -> String -> Text.Text
errorResponse errorCode reason = decodeUtf8 . encode . responseJSON $
  RunResult [] (Just (Diagnostic errorCode reason (pointSpan 1 1))) initialEnv [] False 0

-- Force the complete serialized response inside the deadline. Forcing only the
-- RunResult constructor would let lazy evaluation escape into the HTTP write.
evaluateResponse :: Int -> EvalOptions -> String -> IO Text.Text
evaluateResponse deadline opts source = do
  completed <- timeout deadline $ do
    let payload
          | length (take 65537 source) > 65536 = errorResponse "limit.source" "Source exceeds 65,536 characters"
          | otherwise = decodeUtf8 (encode (responseJSON (runProgram opts initialEnv source)))
    _ <- evaluate (Text.length payload)
    pure payload
  pure $ case completed of
    Nothing -> errorResponse "limit.timeout" "Evaluation exceeded the server deadline"
    Just payload -> payload

runWebServer :: Int -> FilePath -> IO ()
runWebServer port assets = scotty port $ do
  setMaxRequestBodySize 256
  post "/evaluate" $ do
    requestBody <- body
    response <- case decodeUtf8' requestBody of
      Left _ -> pure (errorResponse "input.encoding" "Source must be valid UTF-8")
      Right source -> liftIO (evaluateResponse 5000000 defaultEvalOptions (Text.unpack source))
    text response
    setHeader "Content-Type" "application/json; charset=utf-8"
  get "/" $ do
    built <- liftIO (doesFileExist (assets </> "index.html"))
    if built then do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file (assets </> "index.html")
    else text "Playground assets are missing. Run: cd web-client && npm ci && npm run build"
  get "/assets/:filename" $ do
    filename <- captureParam "filename"
    if filename /= takeFileName filename || filename `elem` [".", ".."] then next
    else do
      exists <- liftIO (doesFileExist (assets </> "assets" </> filename))
      if exists then do
        setHeader "Content-Type" $ case takeExtension filename of
          ".js" -> "text/javascript; charset=utf-8"
          ".css" -> "text/css; charset=utf-8"
          ".woff2" -> "font/woff2"
          ".woff" -> "font/woff"
          ".ttf" -> "font/ttf"
          ".svg" -> "image/svg+xml"
          _ -> "application/octet-stream"
        file (assets </> "assets" </> filename)
      else next
