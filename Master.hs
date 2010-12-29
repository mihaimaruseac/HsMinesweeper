module Master (Time, AgentActivations, World, evolveWorld)
where

import Plane
import Agent

type Time = Int
type AgentActivations a = (Time, AgentState a)
type World a = (Time, Plane, WorldState, [AgentActivations a])

buildWorld :: Int -> Int -> Int -> Int -> World a
buildWorld agents size mines seed
  | size < 5 = error "Field too small"
  | agents == 0 = error "Need at least one agent"
  | agents == 1 = (0, p, iws, [(1, AS (0, 0) size 0 Nothing)])
  | agents == 2 = (0, p, iws, [(1, AS (0, 0) size 0 Nothing),
                               (1, AS (size, size) size 1 Nothing)])
  | agents == 3 = (0, p, iws, [(1, AS (0, 0) size 0 Nothing),
                               (1, AS (0, size) size 1 Nothing),
                               (1, AS (size, size) size 2 Nothing)])
  | agents == 4 = (0, p, iws, [(1, AS (0, 0) size 0 Nothing),
                               (1, AS (0, size) size 1 Nothing),
                               (1, AS (size, 0) size 2 Nothing),
                               (1, AS (size, size) size 3 Nothing)])
  | otherwise = error "Too many agents"
  where
    p = buildPlane size mines seed
    iws = WS []

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateUntil p f i = takeWhile (not . p) . iterate f $ i

evolveWorld :: Int -> Int -> Int -> Int -> Int -> AgentFunction a -> [World a]
evolveWorld tmax agents size mines seed f
  = iterateUntil endOfWorld advanceWorld p0
    where
      p0 = buildWorld agents size mines seed
      endOfWorld (_, _, _, []) = True
      endOfWorld (t, _, _, _) = t > tmax
      advanceWorld (t, p, w, as) = (t + 1, p, w, as)

