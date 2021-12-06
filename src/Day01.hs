module Day01 where

import Shared
import Text.Megaparsec
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Solution
solve = Solution p1 p2
  where
    p1 =
      show
        . countIncreases
        . parseWith parser
    p2 =
      show
        . countIncreases
        . toWindows 3
        . parseWith parser

parser :: Parsec Void Text [Int]
parser = decimal `sepEndBy` newline

countIncreases :: [Int] -> Int
countIncreases = length . filter (uncurry (<)) . (zip <*> drop 1)

toWindows :: Int -> [Int] -> [Int]
toWindows n = map (sum . take n) . (zipWith const <$> tails <*> drop (n - 1))
