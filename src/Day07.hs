module Day07 where

import Shared
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Solution
solve = Solution p1 p2
  where
    p1 =
      show
        . minimum
        . map sum
        . distances
        . parseWith parser
    p2 =
      show
        . minimum
        . map (sum . map nthTriangular)
        . distances
        . parseWith parser

parser :: Parsec Void Text [Int]
parser = decimal `sepEndBy` char ',' <* many newline

domain :: (Bounded a, Enum a, Ord a) => [a] -> [a]
domain = enumFromTo <$> minimum <*> maximum

distances :: [Int] -> [[Int]]
distances crabs = map (\n -> map (abs . subtract n) crabs) (domain crabs)

maximum :: (Bounded a, Ord a) => [a] -> a
maximum = foldl' max minBound

minimum :: (Bounded a, Ord a) => [a] -> a
minimum = foldl' min maxBound

nthTriangular :: Int -> Int
nthTriangular n = n * (n + 1) `div` 2
