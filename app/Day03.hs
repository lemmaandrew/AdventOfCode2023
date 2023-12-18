module Day03
    ( day03p1
    , day03p2
    ) where

import           Data.Char          ( isDigit )
import qualified Data.Map.Strict as M
import           Data.Vector        ( Vector, fromList, (!) )
import           Text.Read          ( readMaybe )

type Row = Int
type Col = Int
type Cursor = (Row, Col)

data Part =
    Part
        { partChar :: Char
        , partRow  :: Row
        , partCol  :: Col
        }
    deriving (Eq, Ord, Show)

nextPartNumberAndParts ::
       Cursor -> Vector (Vector Char) -> (Maybe (Int, [Part]), Cursor)
nextPartNumberAndParts (row, col) grid =
    let numStartColumn = findNumberColumn col
        (num, len) = getNumberAndLen numStartColumn
        parts = getParts numStartColumn len
     in case num of
            Just n
                | not $ null parts ->
                    (Just (n, parts), (row, numStartColumn + len))
                | otherwise -> (Nothing, (row, numStartColumn + len))
            Nothing -> (Nothing, (row, col + 1))
  where
    gridRow = grid ! row
    findNumberColumn =
        until (\c -> c >= length gridRow || isDigit (gridRow ! c)) (+ 1)
    getNumberAndLen numStartColumn =
        let chars =
                [ gridRow ! c
                | c <-
                      takeWhile
                          (\c -> isDigit $ gridRow ! c)
                          [numStartColumn .. length gridRow - 1]
                ]
         in (readMaybe chars, length chars)
    getParts numStartColumn len =
        [ Part (grid ! r ! c) r c
        | r <- [max 0 (row - 1) .. min (length grid - 1) (row + 1)]
        , c <-
              [max 0 (numStartColumn - 1) .. min (length gridRow - 1)
                                                 (numStartColumn + len)]
        , not (isDigit $ grid ! r ! c) && grid ! r ! c /= '.'
        ]

day03p1 :: IO ()
day03p1 = print =<< sum <$> partNumbers
  where
    grid = fromList . map fromList . lines <$> readFile "data/day03.txt"
    cursors =
        zip <$> enumFromTo 0 . subtract 1 . length <$> grid <*>
        return (repeat 0)
    partNumbers = grid >>= \g -> concatMap (getAllNums g) <$> cursors
    getAllNums g cursor =
        let (numAndParts, cursor'@(_, col')) = nextPartNumberAndParts cursor g
         in case numAndParts of
                Nothing
                    | col' < length (g ! 0) -> getAllNums g cursor'
                    | otherwise -> []
                Just (n, _) -> n : getAllNums g cursor'

day03p2 :: IO ()
day03p2 = print =<< sum . fmap product . M.elems <$> gears
  where
    grid = fromList . map fromList . lines <$> readFile "data/day03.txt"
    cursors =
        zip <$> enumFromTo 0 . subtract 1 . length <$> grid <*>
        return (repeat 0)
    gears = grid >>= \g -> M.filter ((== 2) . length)
                         . starMap
                         . concatMap (getAllNumsAndStars g)
                         <$> cursors

    getAllNumsAndStars g cursor =
        let (numAndParts, cursor'@(_, col')) = nextPartNumberAndParts cursor g
         in case numAndParts of
                Nothing
                    | col' < length (g ! 0) -> getAllNumsAndStars g cursor'
                    | otherwise -> []
                Just (n, parts)
                    | '*' `elem` map partChar parts ->
                        (n, [(partRow p, partCol p) | p <- parts, partChar p == '*']) :
                        getAllNumsAndStars g cursor'
                _ -> getAllNumsAndStars g cursor'

    starMap = M.unionsWith (++) . map (uncurry toMap)
      where
        toMap num stars = M.fromList $ zip stars (repeat [num])
