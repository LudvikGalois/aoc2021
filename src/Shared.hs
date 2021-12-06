module Shared where

import Text.Megaparsec

data Solution = Solution {partOne :: Text -> Text, partTwo :: Text -> Text}

parseWith :: Parsec Void Text a -> Text -> a
-- parseWith parser input = fromMaybe (error "Unable to parse input") (parseMaybe parser input)
parseWith parser input = case parse parser "input" input of
  Left err -> error (toText $ errorBundlePretty err)
  Right a -> a
