module Plane (Plane, Coords, Value, buildPlane)
where

import Data.Array
import Data.List
import System.Random

type Plane = Array Coords Value
type Coords = (Int, Int)
type Value = Int

genMines :: Int -> Int -> Int -> [Coords]
genMines seed count max
  | max < 2 = error "Invalid size of field"
  | count <= round (fromIntegral max / 2) = error "Invalid count of mines"
  | count > round (fromIntegral (max * max) / 2) = error "Mines too dense"
  | otherwise = take count $ nub $ take (2 * count) $ zip xs ys
  where
    g = mkStdGen seed
    (g1, g2) = split g
    xs = randomRs (1, max) g1
    ys = randomRs (1, max) g2

placeTicks :: [Coords] -> [(Coords, Value)]
placeTicks bombs = concat $ map placeTickBomb bombs

placeTickBomb :: Coords -> [(Coords, Value)]
placeTickBomb (x, y) = [((x + 1, y), 1),
                        ((x - 1, y), 1),
                        ((x, y + 1), 1),
                        ((x, y - 1), 1),
                        ((x, y), 8)]

buildPlane :: Int -> Int -> Int -> Array Coords Value
buildPlane size mines seed = accumArray (+) 0 ((0, 0), (size, size)) bombs
  where
    minePoss = genMines seed mines (size - 1)
    bombs = placeTicks minePoss

