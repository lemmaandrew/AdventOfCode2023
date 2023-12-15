module Main where

import Day01              ( day01 )
import System.Environment ( getArgs )

days :: [IO ()]
days = [day01]

main :: IO ()
main = do
    [day] <- getArgs
    days !! (read day - 1)
