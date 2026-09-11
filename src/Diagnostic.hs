module Diagnostic
  ( Position (..), SourceSpan (..), Diagnostic (..), pointSpan, renderDiagnostic
  ) where

data Position = Position {line :: Int, column :: Int}
  deriving (Eq, Ord, Show)

data SourceSpan = SourceSpan {start :: Position, end :: Position}
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { code :: String
  , message :: String
  , sourceSpan :: SourceSpan
  } deriving (Eq, Show)

pointSpan :: Int -> Int -> SourceSpan
pointSpan row col = SourceSpan (Position row col) (Position row (col + 1))

renderDiagnostic :: Diagnostic -> String
renderDiagnostic d =
  show (line p) ++ ":" ++ show (column p) ++ ": " ++ message d
  where p = start (sourceSpan d)
