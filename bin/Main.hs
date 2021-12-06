module Main (main) where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import Shared

selectDay :: Int -> Maybe Solution
selectDay 1 = Just Day01.solve
selectDay 2 = Just Day02.solve
selectDay 3 = Just Day03.solve
selectDay 4 = Just Day04.solve
selectDay 5 = Just Day05.solve
selectDay 6 = Just Day06.solve
selectDay _ = Nothing

main :: IO ()
main = do
  [day, part] <- getArgs
  let selectPart = if part == "1" then partOne else partTwo
  case readMaybe day >>= selectDay >>= (pure . selectPart) of
    Just solver -> readFileText "/dev/stdin" >>= putTextLn . solver
    Nothing -> error "Invalid day/part"
