module Day05 where

import qualified Data.Map as M
import Shared
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Solution
solve = Solution p1 p2
  where
    p1 = show . countOverlaps . removeDiagonals . parseWith parser
    p2 = show . countOverlaps . parseWith parser

data Line = Line {start :: (Int, Int), end :: (Int, Int)} deriving stock (Show)

parser :: Parsec Void Text [Line]
parser = some (line <* newline)
  where
    line = Line <$> point <* string " -> " <*> point
    point = (,) <$> decimal <* char ',' <*> decimal

removeDiagonals :: [Line] -> [Line]
removeDiagonals = filter (\(Line (x1, y1) (x2, y2)) -> x1 == x2 || y1 == y2)

countOverlaps :: [Line] -> Int
countOverlaps = length . filter (>= 2) . M.elems . foldl' addPointsOnLine M.empty

addPointsOnLine :: M.Map (Int, Int) Int -> Line -> M.Map (Int, Int) Int
addPointsOnLine m line =
  foldl' (\acc p -> M.insertWith (+) p 1 acc) m (pointsOn line)
  where
    pointsOn :: Line -> [(Int, Int)]
    pointsOn (Line p1@(x1, y1) p2@(x2, y2))
      | p1 == p2 = [p2]
      | otherwise =
        p1 :
        pointsOn
          (Line (x1 - signum (x1 - x2), y1 - signum (y1 - y2)) p2)
