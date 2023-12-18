{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day02
    ( day02p1
    , day02p2
    ) where

import Data.List.Split ( splitOn )

data Game =
    Game
        { gameID :: Int
        , reds   :: Int
        , greens :: Int
        , blues  :: Int
        }
    deriving (Eq, Show)

parseGame :: String -> Game
parseGame line = Game gid maxR maxG maxB
  where
    [idChunk, gameChunk] = splitOn ": " line
    [_, read -> gid] = words idChunk
    rounds = splitOn "; " gameChunk
    getRoundDetails =
        foldr1 (\(r1, g1, b1) (r2, g2, b2) -> (max r1 r2, max g1 g2, max b1 b2)) .
        map parseColor . splitOn ", "
      where
        parseColor colorDetails =
            let [read -> num, color] = words colorDetails
             in case color of
                    "red" -> (num, 0, 0)
                    "green" -> (0, num, 0)
                    "blue" -> (0, 0, num)
                    _ -> error "color should be one of ['red', 'green', 'blue']"
    (maxR, maxG, maxB) =
        foldr1 (\(r1, g1, b1) (r2, g2, b2) -> (max r1 r2, max g1 g2, max b1 b2)) $
        map getRoundDetails rounds

day02p1 :: IO ()
day02p1 = do
    goodGames <-
        filter inBounds . map parseGame . lines <$> readFile "data/day02.txt"
    print . sum $ map gameID goodGames
  where
    inBounds (Game _ r g b) = r <= 12 && g <= 13 && b <= 14

day02p2 :: IO ()
day02p2 = do
    games <- map parseGame . lines <$> readFile "data/day02.txt"
    print . sum $ map power games
  where
    power (Game _ r g b) = r * g * b
