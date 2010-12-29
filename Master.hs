module Master (Time, AgentActivations, World, evolveWorld)
where

import Control.Arrow ((&&&))

import Debug.Trace

import Plane
import Agent

type Time = Int
type AgentActivations a = (Time, AgentState a)
type World a = (Time, Plane, WorldState, [AgentActivations a])

buildWorld :: Int -> Int -> Int -> Int -> World a
buildWorld agents size mines seed
  | size < 5 = error "Field too small"
  | agents == 0 = error "Need at least one agent"
  | agents == 1 = (0, p, iws, [(0, AS (0, 0) size 0 Nothing)])
  | agents == 2 = (0, p, iws, [(0, AS (0, 0) size 0 Nothing),
                               (0, AS (size, size) size 1 Nothing)])
  | agents == 3 = (0, p, iws, [(0, AS (0, 0) size 0 Nothing),
                               (0, AS (0, size) size 1 Nothing),
                               (0, AS (size, size) size 2 Nothing)])
  | agents == 4 = (0, p, iws, [(0, AS (0, 0) size 0 Nothing),
                               (0, AS (0, size) size 1 Nothing),
                               (0, AS (size, 0) size 2 Nothing),
                               (0, AS (size, size) size 3 Nothing)])
  | otherwise = error "Too many agents"
  where
    p = buildPlane size mines seed
    iws = WS []

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateUntil p f = (takeWhile (not . p) . iterate f $)

--evolveWorld :: Int -> Int -> Int -> Int -> Int -> AgentFunction a -> [World a]
evolveWorld tmax agents size mines seed f
  = iterateUntil endOfWorld advanceWorld p0
    where
      p0 = buildWorld agents size mines seed
      endOfWorld (_, _, _, []) = True
      endOfWorld (t, _, _, _) = t > tmax
      advanceWorld (t, p, w, as) = trace ("\n>>" ++ show actionsWithState ++ "<<\n") (t+1, p, w, as')
        where
          activeAgents = map snd $ filter (\x -> fst x == t) as
          actions = map (f w) activeAgents
          actionsWithState = zip (map (asId &&& asPos) activeAgents)
                                 (map fst actions)
          as' = map newStates actionsWithState
          newStates ((id, pos), (a, o)) = (t + 1, AS (newPos a pos) size id o)
          newPos action pos
            | action == Stay = pos
            | otherwise = error "NIY"

{-
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds

t31 :: (a, b, c) -> a
t31 (a, _, _) = a

t32 :: (a, b, c) -> b
t32 (_, b, _) = b

t33 :: (a, b, c) -> c
t33 (_, _, c) = c
--}

