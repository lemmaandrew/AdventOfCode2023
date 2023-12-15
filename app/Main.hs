module Main where

import Day01               ( day01p1, day01p2 )
import Options.Applicative
    ( Parser
    , auto
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , option
    , short
    , (<**>)
    )

days :: [[IO ()]]
days = [[day01p1, day01p2]]

data Args =
    Args
        { day  :: Int
        , part :: Int
        }
    deriving (Eq, Show)

argsParser :: Parser Args
argsParser =
    Args
    <$> option auto (long "day" <> short 'd' <> help "Which day to run")
    <*> option auto (long "part" <> short 'p' <> help "Which part of the day to run")

main :: IO ()
main = do
    Args d p <- execParser opts
    days !! (d - 1) !! (p - 1)
  where
    opts = info (argsParser <**> helper) fullDesc
