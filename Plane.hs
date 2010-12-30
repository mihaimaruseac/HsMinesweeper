module Plane (Plane, Coords, Value, buildPlane, defuse, safePlane,
  getTicks, getBombs, neigh)
where

import Debug.Trace

import Data.Array
import Data.List
import System.Random

type Plane = (Array Coords Value, Bool)
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
placeTicks = concatMap placeTickBomb

placeTickBomb :: Coords -> [(Coords, Value)]
placeTickBomb (x, y) = [((x + 1, y), 1),
                        ((x - 1, y), 1),
                        ((x, y + 1), 1),
                        ((x, y - 1), 1),
                        ((x, y), 8)]

buildPlane :: Int -> Int -> Int -> Plane
buildPlane size mines seed
  = (accumArray (+) 0 ((0, 0), (size, size)) bombs, False)
  where
    minePoss = genMines seed mines (size - 1)
    bombs = placeTicks minePoss

defuse :: Plane -> [Coords] -> Plane
defuse plane positions = (np, planeArray /= np)
  where
    np = accum (-) planeArray $ centers ++ ticks
    planeArray = fst plane
    max = snd $ snd $ bounds planeArray
    neighs = concatMap (neigh max) positions
    centers = map (\x -> (x, 8)) positions
    ticks = map (\x -> (x, 1)) neighs

neigh :: Int -> Coords -> [Coords]
neigh max (x, y) = filter (\(x,y) -> x >= 0 && x <= max && y >= 0 && y <= max)
                   [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

safePlane :: Plane -> Bool
safePlane p = not (snd p) && null (getBombs p)

getTicks :: Plane -> Coords -> Value
getTicks = (!) . fst

getBombs :: Plane -> [Coords]
getBombs = map fst . filter ((>4) . snd) . assocs . fst
