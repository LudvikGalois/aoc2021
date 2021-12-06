module Day04 where

import qualified Data.IntSet as S
import Shared
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Solution
solve = Solution p1 p2
  where
    p1 = show . firstWinner . parseWith parser
    p2 = show . lastWinner . parseWith parser

data BingoGame = BingoGame
  { drawnNumbers :: [Int],
    boards :: [BingoBoard]
  }
  deriving stock (Show)

type BingoBoard = [[Int]]

parser :: Parsec Void Text BingoGame
parser = BingoGame <$> numbers <*> some board
  where
    numbers = decimal `sepBy` char ',' <* some newline
    board = count 5 boardRow
    boardRow = count 5 (many (char ' ') *> decimal) <* some newline

winningBoard :: BingoBoard -> IntSet -> Bool
winningBoard board drawn =
  any (all (`S.member` drawn)) (board ++ transpose board)

score :: BingoBoard -> IntSet -> Int -> Int
score board drawn lastDrawn =
  lastDrawn
    * sum (filter (not . (`S.member` drawn)) (concat board))

firstWinner :: BingoGame -> Int
firstWinner (BingoGame ns bs) = go S.empty ns bs
  where
    go _ [] _ = error "No winning boards"
    go previous (next : toDraw) boards =
      let drawn = S.insert next previous
          winners = filter (`winningBoard` drawn) boards
       in case winners of
            (winner : _) -> score winner drawn next
            [] -> go drawn toDraw boards

lastWinner :: BingoGame -> Int
lastWinner (BingoGame ns bs) = go S.empty ns bs
  where
    go _ [] _ = error "No winning boards"
    go previous (next : toDraw) boards =
      let drawn = S.insert next previous
       in case filter (not . (`winningBoard` drawn)) boards of
            [] ->
              score
                ( fromMaybe (error "lastWinner: No boards") $
                    viaNonEmpty head boards
                )
                drawn
                next
            remainingBoards@_ -> go drawn toDraw remainingBoards
