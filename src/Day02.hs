module Day02 where

import Shared
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Solution
solve = Solution p1 p2
  where
    p1 =
      show
        . uncurry (*)
        . foldl' (&) (0, 0)
        . map toMovement
        . parseWith parser
    p2 =
      show
        . uncurry (*)
        . submarinePosition
        . foldl' (&) (Submarine 0 (0, 0))
        . map submarineMovement
        . parseWith parser

data Instruction = F Int | U Int | D Int

parser :: Parsec Void Text [Instruction]
parser = (instruction <*> (space1 *> decimal)) `sepEndBy` newline
  where
    instruction :: Parsec Void Text (Int -> Instruction)
    instruction =
      string "forward" $> F
        <|> string "up" $> U
        <|> string "down" $> D

toMovement :: Instruction -> (Int, Int) -> (Int, Int)
toMovement instr (x, y) = case instr of
  F n -> (x + n, y)
  U n -> (x, y - n)
  D n -> (x, y + n)

data Submarine = Submarine
  { submarineAim :: Int,
    submarinePosition :: (Int, Int)
  }
  deriving stock (Show)

submarineMovement :: Instruction -> Submarine -> Submarine
submarineMovement instr (Submarine aim (x, y)) = case instr of
  F n -> Submarine aim (x + n, y + aim * n)
  U n -> Submarine (aim - n) (x, y)
  D n -> Submarine (aim + n) (x, y)
