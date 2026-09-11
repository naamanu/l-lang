{-# LANGUAGE OverloadedStrings #-}
module Examples (Example (..), loadExamples) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict', withObject, (.:))
import Paths_l_lang (getDataFileName)

data Example = Example
  { exampleId :: String
  , exampleTitle :: String
  , exampleDescription :: String
  , exampleCode :: String
  , exampleExpected :: String
  } deriving (Show)

instance FromJSON Example where
  parseJSON = withObject "Example" $ \o -> Example
    <$> o .: "id" <*> o .: "title" <*> o .: "description"
    <*> o .: "code" <*> o .: "expected"

loadExamples :: IO [Example]
loadExamples = do
  path <- getDataFileName "examples/programs.json"
  decoded <- eitherDecodeFileStrict' path
  either (ioError . userError) pure decoded
