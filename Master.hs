module Master (Time, AgentActivations, World, evolveWorld)
where

import Control.Arrow ((&&&))
import Data.Array
import Data.List

import Debug.Trace

import Agent
import Plane

type Time = Int
type AgentActivations a = (Time, AgentState a)
type AgentBombs = (AgentID, Int)
type World a = (Time, Plane, WorldState, [AgentBombs], [AgentActivations a])
type AgentActivity a = ((AgentID, Coords), (Move, Maybe a))

buildWorld :: Int -> Int -> Int -> Int -> World a
buildWorld agents size mines seed
  | size < 5 = error "Field too small"
  | agents == 0 = error "Need at least one agent"
  | agents == 1 = (0, p, iws, [], [(0, AS (0, 0) size 0 0 Nothing)])
  | agents == 2 = (0, p, iws, [], [(0, AS (0, 0) size 0 0 Nothing),
                                   (0, AS (size, size) size 1 0 Nothing)])
  | agents == 3 = (0, p, iws, [], [(0, AS (0, 0) size 0 0 Nothing),
                                   (0, AS (0, size) size 1 0 Nothing),
                                   (0, AS (size, size) size 2 0 Nothing)])
  | agents == 4 = (0, p, iws, [], [(0, AS (0, 0) size 0 0 Nothing),
                                   (0, AS (0, size) size 1 0 Nothing),
                                   (0, AS (size, 0) size 2 0 Nothing),
                                   (0, AS (size, size) size 3 0 Nothing)])
  | otherwise = error "Too many agents"
  where
    p = buildPlane size mines seed
    iws = WS []

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateUntil p f = (takeWhile (not . p) . iterate f $)

evolveWorld :: (Eq a) => Int -> Int -> Int -> Int -> Int -> AgentFunction a -> [World a]
evolveWorld tmax agents size mines seed f
  = iterateUntil endOfWorld (advanceWorld size f) p0
    where
      p0 = buildWorld agents size mines seed
      endOfWorld (_, _, _, _, []) = True
      endOfWorld (t, p, _, _, _) = t > tmax || safePlane p

advanceWorld :: (Eq a) => Int -> AgentFunction a -> World a -> World a
advanceWorld size f (t, p, w, bs, as) = (t+1, p', w', bs', as')
  where
    activeAgents = map snd $ filter (\x -> fst x == t) as
    actions = map (f w) activeAgents
    actionsState = zip (map (asId &&& asPos) activeAgents)
                           (map fst actions)
    later = removeDuplicatesBy notSameDefuse $ filter (\x -> fst x /= t) as
    notSameDefuse (_, as1) (_, as2) = asPos as1 == asPos as2
    as' = concatMap (newSt t size p) actionsState ++ later
    defusedPoss = filter (\c -> getTicks p c > 7) $ map (asPos.snd) later
    p' = defuse p defusedPoss
    agentsDefusing = map (asId.snd &&& asPos.snd) later
    success = map fst $ filter (\(_,y)->y `elem` defusedPoss) agentsDefusing
    bs' = addSuccesses success bs
    w' = foldl1 combineWS $ map snd actions

newSt :: Time -> Int -> Plane -> AgentActivity a -> [AgentActivations a]
newSt t size pl ((id, p), (Stay, o))
  = [(t + 1, AS p size id (trim $ getTicks pl p) o)]
newSt t size pl ((id, p), ((Move np), o))
  = [(t + 1, AS np size id (trim $ getTicks pl np) o) | safe np]
  where
    safe pos = getTicks pl pos < 5
newSt t size pl ((id, p), ((MoveAndDefuse np), o))
  = [(t + 2, AS np size id (trim $ getTicks pl np) o)]

removeDuplicatesBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
removeDuplicatesBy f l = filter (not.elemBy f (l \\ nubBy f l)) l
  where
    elemBy f ys x = foldr ((||) . f x) False ys

trim :: Value -> Value
trim v = if v > 7 then v - 8 else v

addSuccesses :: [AgentID] -> [AgentBombs] -> [AgentBombs]
addSuccesses a b = nubBy (\x y -> fst x == fst y) $ map (addSuccess b) a ++ b

addSuccess :: [AgentBombs] -> AgentID -> AgentBombs
addSuccess bs id = case lookup id bs of
  Nothing -> (id, 1)
  Just v -> (id, v + 1)

