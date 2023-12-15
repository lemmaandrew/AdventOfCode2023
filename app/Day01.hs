module Day01
    ( day01
    ) where

import Data.Char ( isDigit )

getCalibrationValue :: String -> Int
getCalibrationValue line = read [head digits, last digits]
  where
    digits = filter isDigit line

day01 :: IO ()
day01 =
    print =<<
    sum . map getCalibrationValue . lines <$> readFile "data/day01.txt"
