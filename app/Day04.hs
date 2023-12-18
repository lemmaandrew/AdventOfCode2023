{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards #-}

module Day04
    ( day04p1
    , day04p2
    ) where

import Data.Bifunctor        ( second )
import Data.Char             ( isDigit )
import qualified Data.IntSet as IS

data Card =
    Card
        { cardNum :: Int
        , winners :: [Int]
        , playing :: [Int]
        }
    deriving (Eq, Show)

parseInput :: IO [Card]
parseInput = map toCard . lines <$> readFile "data/day04.txt"
  where
    toCard line =
        let (cardNumPart, ':':cardPart) = span (/= ':') line
            cardNum = read $ filter isDigit cardNumPart
            (winnersPart, '|':playingPart) = span (/= '|') cardPart
            winners = map read $ words winnersPart
            playing = map read $ words playingPart
         in Card {..}

day04p1 :: IO ()
day04p1 = print =<< (sum . map getScore <$> cards :: IO Int)
  where
    cards = parseInput

    getScore (Card _ w p) =
        case IS.size $ IS.fromList w `IS.intersection` IS.fromList p of
            0 -> 0
            n -> 2 ^ (n - 1)

day04p2 :: IO ()
day04p2 = print =<< (loop . flip zip (repeat 1) <$> cards :: IO Int)
  where
    cards = parseInput

    loop [] = 0
    loop ((card, n) : cardsAndQuantities) =
        let (toIncrement, rest) = splitAt (numMatching card) cardsAndQuantities
            incremented = map (second (+n)) toIncrement
        in n + loop (incremented ++ rest)

    numMatching (Card _ w p) = IS.size $ IS.fromList w `IS.intersection` IS.fromList p
