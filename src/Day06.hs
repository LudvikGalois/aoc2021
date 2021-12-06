module Day06 where

import qualified Data.Map as M
import Shared
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Solution
solve = Solution p1 p2
  where
    p1 = show . sum . M.elems . doTimes 80 fish . parseWith parser
    p2 = show . sum . M.elems . doTimes 256 fish . parseWith parser

parser :: Parsec Void Text (M.Map Int Integer)
parser =
  foldl' (\acc n -> M.insertWith (+) n 1 acc) M.empty
    <$> (decimal `sepEndBy` char ',' <* many newline)

fish :: M.Map Int Integer -> M.Map Int Integer
fish =
  M.foldrWithKey
    ( \k n acc ->
        if k == 0
          then M.insertWith (+) 8 n (M.insertWith (+) 6 n acc)
          else M.insertWith (+) (k -1) n acc
    )
    M.empty

doTimes :: Int -> (a -> a) -> a -> a
doTimes 1 f = f
doTimes n f = f . doTimes (n -1) f
