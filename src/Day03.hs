module Day03 where

import Shared
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char (char, newline)

solve :: Solution
solve = Solution p1 p2
  where
    p1 =
      show
        . ((*) <$> bitsToInt <*> (bitsToInt . map not))
        . map ((== One) . mostCommon)
        . transpose
        . parseWith parser
    p2 =
      show
        . ((*) <$> ratingFor Oxygen <*> ratingFor CO2)
        . parseWith parser

parser :: Parsec Void Text [[Bool]]
parser = (some bit) `sepEndBy` newline
  where
    bit = char '0' $> False <|> char '1' $> True

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' (\acc p -> 2 * acc + (if p then 1 else 0)) 0

data MostCommonBit = Zero | Equal | One deriving stock (Eq)

mostCommon :: [Bool] -> MostCommonBit
mostCommon bits = case compare (length bits) (2 * length (filter id bits)) of
  LT -> One
  EQ -> Equal
  GT -> Zero

data RatingType = Oxygen | CO2 deriving stock (Eq)

ratingFor :: RatingType -> [[Bool]] -> Int
ratingFor ratingType = go []
  where
    choose = if ratingType == Oxygen then (/= Zero) else (== Zero)
    go _ [] = error "ratingFor: The impossible happened"
    go consumed [val] = bitsToInt (reverse consumed ++ val)
    go consumed vals =
      let chosen = choose $ mostCommon $ catMaybes $ map (viaNonEmpty head) vals
          remaining = filter (\x -> Just chosen == viaNonEmpty head x) vals
       in go (chosen : consumed) (map (drop 1) remaining)
