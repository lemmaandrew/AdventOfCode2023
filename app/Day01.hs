module Day01
    ( day01p1
    , day01p2
    ) where

import Data.Char ( digitToInt, isDigit )

getCalibrationValue :: String -> Int
getCalibrationValue line = read [head digits, last digits]
  where
    digits = filter isDigit line

day01p1 :: IO ()
day01p1 =
    print
    =<< sum
    . map getCalibrationValue
    . lines
    <$> readFile "data/day01.txt"

day01p2 :: IO ()
day01p2 = sum <$> calibrationValues >>= print
  where
    fileLines = lines <$> readFile "data/day01.txt"

    -- parses all the digits of a line, including the words
    parseLine [] = []
    parseLine ('o':'n':'e':xs) = 1 : parseLine ('e' : xs)
    parseLine ('t':'w':'o':xs) = 2 : parseLine ('o' : xs)
    parseLine ('t':'h':'r':'e':'e':xs) = 3 : parseLine ('e' : xs)
    parseLine ('f':'o':'u':'r':xs) = 4 : parseLine xs
    parseLine ('f':'i':'v':'e':xs) = 5 : parseLine ('e' : xs)
    parseLine ('s':'i':'x':xs) = 6 : parseLine xs
    parseLine ('s':'e':'v':'e':'n':xs) = 7 : parseLine ('n' : xs)
    parseLine ('e':'i':'g':'h':'t':xs) = 8 : parseLine ('t' : xs)
    parseLine ('n':'i':'n':'e':xs) = 9 : parseLine ('e' : xs)
    parseLine (x:xs)
        | isDigit x = digitToInt x : parseLine xs
        | otherwise = parseLine xs

    parsedLines = map parseLine <$> fileLines

    calibrationValues = map (\line -> head line * 10 + last line) <$> parsedLines
